calculate_deg_sep <- function(actor_network,
                              distance_mat,
                              from_name) {
  actor_network %>%
    activate(nodes) %>%
    mutate(
      steps_from_start_node =
        distance_mat[from_name, ] %>%
          as.character()
    )
}

calculate_shortest_path <- function(actor_network,
                                    from_name,
                                    to_name) {
  from_ix <- actor_network %>%
    activate(nodes) %>%
    pull(name) %>%
    purrr::detect_index(~ . == from_name)

  to_ix <- actor_network %>%
    activate(nodes) %>%
    pull(name) %>%
    purrr::detect_index(~ . == to_name)

  # initiate `selected` columns
  actor_network <-
    actor_network %>%
    activate(nodes) %>%
    mutate(selected_node = 0) %>%
    activate(edges) %>%
    mutate(selected_edge = 0)

  # calculate a shortest path for two nodes
  actor_network %>%
    morph(to_shortest_path, from = from_ix, to = to_ix) %>%
    activate(nodes) %>%
    mutate(selected_node = 1) %>%
    activate(edges) %>%
    mutate(selected_edge = 1) %>%
    unmorph()
}

draw_deg_sep <- function(actor_network) {
  ggraph(actor_network, layout = "kk") +
    geom_edge_fan(
      lineend = "round",
      show.legend = FALSE,
      alpha = 0.1,
      edge_width = 1
    ) +
    geom_node_label(aes(label = name, fill = steps_from_start_node),
      size = 4,
      colour = "white"
    ) +
    scale_fill_manual(
      values = viridis::viridis(n = 5, end = 0.8, direction = -1, option = "A")
    ) +
    scale_y_continuous(limits = c(-3, 3)) +
    labs(fill = "") +
    theme_graph() +
    theme(plot.margin = unit(c(0, 0, 0, 0), "cm"))
}

draw_shortest_path <- function(actor_network) {
  ggraph(actor_network, layout = "kk") +
    geom_edge_fan(aes(
      alpha = selected_edge,
      color = selected_edge,
      edge_width = selected_edge
    ),
    lineend = "round",
    show.legend = FALSE
    ) +
    geom_node_label(
      aes(
        label = name,
        fill = steps_from_start_node,
        alpha = selected_node
      ),
      size = 4,
      colour = "white"
    ) +
    scale_edge_alpha(range = c(0.04, 1)) +
    scale_edge_width(range = c(1, 2)) +
    scale_alpha(range = c(0.2, 1), guide = FALSE) +
    scale_fill_manual(
      values = viridis::viridis(n = 5, end = 0.8, direction = -1, option = "A")
    ) +
    scale_y_continuous(limits = c(-3, 3)) +
    labs(fill = "") +
    theme_graph() +
    theme(plot.margin = unit(c(0, 0, 0, 0), "cm"),
          panel.background = element_rect(fill = "#FFFFFF", colour = "#FFFFFF"))
}

draw_sub_graph <- function(actor_network, from_name, to_name) {
  sub_network <-
    actor_network %>%
    activate(nodes) %>%
    filter(selected_node == 1) %>%
    activate(edges) %>%
    filter(selected_edge == 1)

  node_sequence <-
    igraph::shortest_paths(
      sub_network,
      from = from_name,
      to = to_name
    ) %>%
    purrr::pluck("vpath") %>%
    purrr::flatten() %>%
    names()

  sub_network <-
    sub_network %>%
    activate(nodes) %>%
    mutate(node_order = purrr::map_dbl(name, ~ which(. == node_sequence))) %>%
    arrange(node_order) %>%
    to_directed()

  ggraph(sub_network, layout = "linear") +
    geom_edge_link(
      colour = "#AAAAAA",
      edge_width = 1.5,
      arrow = arrow(length = unit(4, "mm")),
      end_cap = circle(25, "mm"),
      start_cap = circle(25, "mm")
    ) +
    geom_node_label(
      aes(label = name, fill = as.character(node_order)),
      size = 4,
      colour = "white",
      label.r = unit(0.5, "lines"),
      label.padding = unit(0.5, "lines")
    ) +
    scale_fill_manual(
      values = viridis::viridis(n = 5, end = 0.8, direction = -1, option = "A"),
      guide = FALSE
    ) +
    # scale_y_continuous(limits = c(-1, 0.1)) +
    theme_graph() +
    theme(plot.margin = unit(c(5, 5, 5, 5), "cm"),
          panel.background = element_rect(fill = "#FFFFFF", colour = "#FFFFFF"))
}
