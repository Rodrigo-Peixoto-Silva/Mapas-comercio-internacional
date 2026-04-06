rm(list = ls())

# ============================================================
# 0. PACOTES
# ============================================================
pacotes <- c(
  "tidyverse",
  "sf",
  "rnaturalearth",
  "rnaturalearthdata",
  "readxl",
  "tcltk",
  "grid"
)

faltantes <- pacotes[!pacotes %in% installed.packages()[, "Package"]]

if (length(faltantes) > 0) {
  install.packages(faltantes)
}

invisible(lapply(pacotes, library, character.only = TRUE))

# Desativa s2 para garantir união dos polígonos globais
sf::sf_use_s2(FALSE)

# ============================================================
# 1. DIRETÓRIO DE TRABALHO
# ============================================================
possiveis_drives <- c("G:/", "H:/", "I:/")

caminho_relativo <- "Drives compartilhados/Macro Cepea/Cadeias/SOJA/Relatórios"

caminho_final <- NULL

for (d in possiveis_drives) {
  caminho_teste <- file.path(d, caminho_relativo)
  
  if (dir.exists(caminho_teste)) {
    caminho_final <- caminho_teste
    break
  }
}

if (is.null(caminho_final)) {
  stop("Não foi possível encontrar o diretório do projeto. Verifique o acesso ao Drive.")
}

setwd(caminho_final)

message("Diretório definido como: ", caminho_final)

# ============================================================
# 2. FUNÇÃO PARA SELEÇÃO INTERATIVA DE ANO E TRIMESTRE
# ============================================================
selecionar_periodo <- function(base) {
  
  anos_disponiveis <- base |>
    dplyr::pull(Ano) |>
    unique() |>
    sort()
  
  ano_escolhido <- tcltk::tk_select.list(
    choices = as.character(anos_disponiveis),
    title = "Selecione o ano",
    multiple = FALSE
  )
  
  if (ano_escolhido == "") {
    stop("Operação cancelada: nenhum ano foi selecionado.")
  }
  
  trimestres_disponiveis <- base |>
    dplyr::filter(Ano == as.integer(ano_escolhido)) |>
    dplyr::pull(Trimestre) |>
    unique() |>
    sort()
  
  trimestre_escolhido <- tcltk::tk_select.list(
    choices = paste0(trimestres_disponiveis, "º trimestre"),
    title = paste("Selecione o trimestre de", ano_escolhido),
    multiple = FALSE
  )
  
  if (trimestre_escolhido == "") {
    stop("Operação cancelada: nenhum trimestre foi selecionado.")
  }
  
  trimestre_num <- as.integer(gsub("[^0-9]", "", trimestre_escolhido))
  
  list(
    ano = as.integer(ano_escolhido),
    trimestre = trimestre_num
  )
}

# ============================================================
# 3. FUNÇÃO PARA GERAR NOME DE ARQUIVO
# ============================================================
normalizar_nome_produto <- function(produto) {
  dplyr::case_when(
    produto == "Soja" ~ "soja",
    produto == "Farelo de soja" ~ "farelo_soja",
    produto == "Óleo de soja" ~ "oleo_soja",
    TRUE ~ produto |>
      stringr::str_to_lower() |>
      iconv(from = "UTF-8", to = "ASCII//TRANSLIT") |>
      stringr::str_replace_all("[^a-z0-9]+", "_") |>
      stringr::str_replace_all("^_|_$", "")
  )
}

# ============================================================
# 4. FUNÇÃO PARA CRIAR MAPA
# ============================================================
criar_mapa_fluxo <- function(produto,
                             paises_coloridos,
                             grupos_coords,
                             cores_grupos,
                             ano,
                             trimestre,
                             com_rotulo = TRUE) {
  
  dados_produto <- grupos_coords |>
    dplyr::filter(Produto == produto) |>
    dplyr::filter(!is.na(toneladas), toneladas > 0)
  
  p <- ggplot() +
    geom_sf(
      data = paises_coloridos,
      aes(fill = grupo_plot),
      color = "gray10",
      size = 0.2
    ) +
    geom_curve(
      data = dados_produto,
      aes(
        x = lon_origem,
        y = lat_origem,
        xend = lon_destino,
        yend = lat_destino,
        linewidth = log(toneladas) + 1
      ),
      colour = "black",
      curvature = -0.2,
      arrow = arrow(length = unit(0.4, "cm"), type = "closed", angle = 15),
      show.legend = FALSE
    ) +
    geom_curve(
      data = dados_produto,
      aes(
        x = lon_origem,
        y = lat_origem,
        xend = lon_destino,
        yend = lat_destino,
        color = Grupo_2,
        linewidth = log(toneladas)
      ),
      curvature = -0.2,
      arrow = arrow(length = unit(0.4, "cm"), type = "closed", angle = 15),
      show.legend = FALSE
    ) +
    coord_sf() +
    scale_fill_manual(values = cores_grupos, na.value = "gray90") +
    scale_color_manual(values = cores_grupos) +
    theme_minimal() +
    labs(
      title = paste0("Exportações de ", produto, " (Toneladas)"),
      subtitle = paste0(trimestre, "º Trimestre de ", ano),
      fill = "Grupo de Países",
      color = "Grupo de Países",
      linewidth = "log(toneladas)",
      x = "",
      y = ""
    ) +
    theme(
      legend.position = "bottom",
      plot.background = element_rect(fill = "white", color = NA)
    )
  
  if (com_rotulo) {
    p <- p +
      geom_label(
        data = dados_produto,
        aes(
          x = lon_destino,
          y = lat_destino,
          label = scales::label_number(
            scale_cut = scales::cut_short_scale(),
            accuracy = 0.1
          )(toneladas)
        ),
        color = "black",
        fill = "white",
        size = 3,
        nudge_y = 5,
        label.size = 0.2
      )
  }
  
  p
}

# ============================================================
# 5. FUNÇÃO PARA SALVAR MAPA
# ============================================================
salvar_mapa <- function(plot, dir_saida, produto, com_rotulo = TRUE) {
  
  nome_base <- normalizar_nome_produto(produto)
  sufixo <- ifelse(com_rotulo, "bspline", "bspline_sr")
  
  caminho_arquivo <- file.path(
    dir_saida,
    paste0("mapa_", nome_base, "_", sufixo, ".png")
  )
  
  ggsave(
    filename = caminho_arquivo,
    plot = plot,
    width = 12,
    height = 8,
    dpi = 500
  )
  
  message("Arquivo salvo em: ", caminho_arquivo)
}

# ============================================================
# 6. LEITURA DA BASE COMPLETA
# ============================================================
base_completa <- readxl::read_xlsx(
  "Comércio Internacional.xlsx",
  sheet = "Resultado"
)

# ============================================================
# 7. SELEÇÃO INTERATIVA DO PERÍODO
# ============================================================
periodo <- selecionar_periodo(base_completa)

ano <- periodo$ano
trimestre <- periodo$trimestre

# ============================================================
# 8. FILTRAGEM DOS DADOS
# ============================================================
dados <- base_completa |>
  dplyr::filter(
    Ano == ano,
    Trimestre == trimestre,
    Fluxo == "Exportação"
  )

# ============================================================
# 9. AGREGAÇÕES
# ============================================================
dados_agrupados <- dados |>
  dplyr::group_by(Produto, iso_a3, Países, Grupo_2) |>
  dplyr::summarise(
    valor_fob = sum(`Valor FOB (US$)`, na.rm = TRUE),
    toneladas = sum(`Quilograma Líquido`, na.rm = TRUE) / 1000,
    .groups = "drop"
  )

dados_agrupados_2 <- dados |>
  dplyr::group_by(Produto, Grupo_2) |>
  dplyr::summarise(
    valor_fob = sum(`Valor FOB (US$)`, na.rm = TRUE),
    toneladas = sum(`Quilograma Líquido`, na.rm = TRUE) / 1000,
    .groups = "drop"
  )

# ============================================================
# 10. MAPA MUNDIAL
# ============================================================
paises <- rnaturalearth::ne_countries(returnclass = "sf") |>
  st_transform(crs = 4326)

# ============================================================
# 11. GRUPOS E CORES
# ============================================================
levels_grupos <- c(
  "África",
  "América do Norte",
  "China",
  "Índia",
  "Leste Asiático",
  "Oriente Médio",
  "Sudeste Asiático",
  "União Europeia",
  "Outros"
)

cores_grupos <- c(
  "África" = "#1b9e77",
  "América do Norte" = "#d95f02",
  "China" = "#7570b3",
  "Índia" = "cyan",
  "Leste Asiático" = "#e7298a",
  "Oriente Médio" = "#66a61e",
  "Sudeste Asiático" = "#e6ab02",
  "União Europeia" = "brown",
  "Outros" = "khaki1"
)

# ============================================================
# 12. COLORAÇÃO DOS PAÍSES
# ============================================================
paises_coloridos <- paises |>
  dplyr::left_join(
    dados_agrupados |>
      dplyr::distinct(iso_a3, Grupo_2),
    by = "iso_a3"
  ) |>
  dplyr::mutate(
    grupo_plot = factor(
      dplyr::if_else(is.na(Grupo_2), "NA", Grupo_2),
      levels = levels_grupos
    )
  )

# ============================================================
# 13. COORDENADAS DE ORIGEM (BRASIL)
# ============================================================
origem_wgs84 <- paises |>
  dplyr::filter(iso_a3 == "BRA") |>
  st_point_on_surface() |>
  st_coordinates() |>
  as_tibble() |>
  dplyr::rename(
    lon_origem = X,
    lat_origem = Y
  )

# ============================================================
# 14. COORDENADAS DE DESTINO
# ============================================================
destinos <- dados_agrupados |>
  dplyr::distinct(iso_a3) |>
  dplyr::inner_join(paises, by = "iso_a3") |>
  dplyr::mutate(geometry = st_point_on_surface(geometry))

coords_destinos <- st_coordinates(destinos$geometry)

destinos <- destinos |>
  dplyr::mutate(
    lon_destino = coords_destinos[, 1],
    lat_destino = coords_destinos[, 2]
  ) |>
  dplyr::select(iso_a3, lon_destino, lat_destino)

# ============================================================
# 15. JUNÇÃO DAS COORDENADAS AOS DADOS
# ============================================================
dados_com_coords <- dados_agrupados |>
  dplyr::left_join(destinos, by = "iso_a3") |>
  dplyr::mutate(
    lon_origem = origem_wgs84$lon_origem,
    lat_origem = origem_wgs84$lat_origem
  )

# ============================================================
# 16. AGREGAÇÃO DOS FLUXOS POR GRUPO
# ============================================================
grupos_coords <- dados_com_coords |>
  dplyr::filter(Grupo_2 != "Outros") |>
  tidyr::drop_na(lon_destino, lat_destino) |>
  dplyr::group_by(Grupo_2, Produto) |>
  dplyr::summarise(
    lon_destino = weighted.mean(lon_destino, valor_fob, na.rm = TRUE),
    lat_destino = weighted.mean(lat_destino, valor_fob, na.rm = TRUE),
    .groups = "drop"
  ) |>
  dplyr::mutate(
    lon_origem = origem_wgs84$lon_origem,
    lat_origem = origem_wgs84$lat_origem
  ) |>
  dplyr::left_join(
    dados_agrupados_2,
    by = c("Grupo_2", "Produto")
  )

# ============================================================
# 17. CRIAÇÃO AUTOMÁTICA DA PASTA DE SAÍDA
# ============================================================
dir_saida <- file.path(
  "Figuras Comércio Exterior/Trimestrais",
  paste0(ano, "_", trimestre)
)

if (!dir.exists(dir_saida)) {
  dir.create(dir_saida, recursive = TRUE)
}

# ============================================================
# 18. PRODUTOS A SEREM PROCESSADOS
# ============================================================
produtos <- c(
  "Soja",
  "Farelo de soja",
  "Óleo de soja"
)

# ============================================================
# 19. GERAÇÃO E SALVAMENTO DOS MAPAS
# ============================================================
for (produto_atual in produtos) {
  
  message("Gerando mapas para: ", produto_atual)
  
  # mapa com rótulo
  mapa_com_rotulo <- criar_mapa_fluxo(
    produto = produto_atual,
    paises_coloridos = paises_coloridos,
    grupos_coords = grupos_coords,
    cores_grupos = cores_grupos,
    ano = ano,
    trimestre = trimestre,
    com_rotulo = TRUE
  )
  
  salvar_mapa(
    plot = mapa_com_rotulo,
    dir_saida = dir_saida,
    produto = produto_atual,
    com_rotulo = TRUE
  )
  
  # mapa sem rótulo
  mapa_sem_rotulo <- criar_mapa_fluxo(
    produto = produto_atual,
    paises_coloridos = paises_coloridos,
    grupos_coords = grupos_coords,
    cores_grupos = cores_grupos,
    ano = ano,
    trimestre = trimestre,
    com_rotulo = FALSE
  )
  
  salvar_mapa(
    plot = mapa_sem_rotulo,
    dir_saida = dir_saida,
    produto = produto_atual,
    com_rotulo = FALSE
  )
}

message("Processamento concluído com sucesso.")
