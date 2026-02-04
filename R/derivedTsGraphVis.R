library(htmlwidgets)
library(visNetwork)
library(widgetframe)

#' Create Timeseries Operator Graph Nodes
#'
#' Prepares a nodes data frame for visNetwork from timeseries operator data,
#' with optional highlighting of specified nodes.
#'
#' @param tsops_nodes Data frame with columns: tag, bandpass, description
#' @param highlight_tags Character vector of node tags to highlight (default: NULL)
#' @param highlight_border_width Border width for highlighted nodes (default: 5)
#' @param normal_border_width Border width for normal nodes (default: 1)
#' @param highlight_font_size Font size for highlighted nodes (default: 16)
#' @param normal_font_size Font size for normal nodes (default: 11)
#' @return Data frame suitable for visNetwork nodes parameter
#' @export
create_tsops_nodes <- function(tsops_nodes,
                               highlight_tags = NULL,
                               highlight_border_width = 7,
                               normal_border_width = 1,
                               highlight_font_size = 17,
                               normal_font_size = 11) {

  if (is.null(highlight_tags)) highlight_tags <- character(0)

  data.frame(
    id = tsops_nodes$tag,
    label = tsops_nodes$tag,
    group = tsops_nodes$bandpass,
    shape = "box",
    title = paste0(tsops_nodes$tag, ":<p><b> ", tsops_nodes$description, "</b><br></p>"),
    shadow = TRUE,
    borderWidth = ifelse(tsops_nodes$tag %in% highlight_tags,
                         highlight_border_width, normal_border_width),
    font.size = ifelse(tsops_nodes$tag %in% highlight_tags,
                       highlight_font_size, normal_font_size),
    stringsAsFactors = FALSE
  )
}


#' Create Timeseries Operator Graph Edges
#'
#' Prepares an edges data frame for visNetwork from timeseries operator data.
#'
#' @param tsops Data frame with columns: inputtag, branch
#' @param arrows Arrow direction (default: "to")
#' @param dashes Logical, use dashed lines (default: TRUE)
#' @param smooth Logical, use smooth curves (default: FALSE)
#' @param shadow Logical, show shadow (default: TRUE)
#' @return Data frame suitable for visNetwork edges parameter
#' @export
create_tsops_edges <- function(tsops,
                               arrows = "to",
                               dashes = TRUE,
                               smooth = FALSE,
                               shadow = TRUE) {

  data.frame(
    from = tsops$inputtag,
    to = tsops$branch,
    arrows = arrows,
    dashes = dashes,
    smooth = smooth,
    shadow = shadow,
    stringsAsFactors = FALSE
  )
}


#' Filter Graph to Connected Components
#'
#' Filters nodes and edges to only include components containing specified nodes.
#'
#' @param nodes Data frame of nodes with 'id' column
#' @param edges Data frame of edges with 'from' and 'to' columns
#' @param target_nodes Character vector of node IDs to keep (along with their components)
#' @return List with filtered 'nodes' and 'edges' data frames
#' @export
#' @importFrom igraph graph_from_data_frame components
filter_graph_components <- function(nodes, edges, target_nodes) {

  if (!requireNamespace("igraph", quietly = TRUE)) {
    stop("Package 'igraph' is required. Install with: install.packages('igraph')")
  }

  g <- igraph::graph_from_data_frame(edges, directed = TRUE, vertices = nodes$id)
  comp <- igraph::components(g)

  # Find components containing target nodes
  target_components <- unique(comp$membership[target_nodes])
  nodes_to_keep <- names(comp$membership)[comp$membership %in% target_components]

  # Filter
  nodes_filtered <- nodes[nodes$id %in% nodes_to_keep, ]
  edges_filtered <- edges[edges$from %in% nodes_to_keep & edges$to %in% nodes_to_keep, ]

  list(nodes = nodes_filtered, edges = edges_filtered)
}


#' Create Timeseries Operator visNetwork Graph
#'
#' Creates an interactive visNetwork graph for timeseries operators with
#' hierarchical layout and optional node highlighting.
#'
#' @param nodes Data frame of nodes (from create_tsops_nodes or similar)
#' @param edges Data frame of edges (from create_tsops_edges or similar)
#' @param width Widget width in pixels (default: 1000)
#' @param height Widget height in pixels (default: 600)
#' @param main Plot title (default: "Timeseries operator graph")
#' @param level_separation Horizontal spacing between hierarchy levels (default: 400)
#' @param node_spacing Vertical spacing between nodes (default: 150)
#' @param tree_spacing Spacing between disconnected trees (default: 50)
#' @param fit_on_stabilize Logical, fit view after stabilization (default: TRUE)
#' @return visNetwork widget
#' @export
#' @importFrom visNetwork visNetwork visOptions visHierarchicalLayout visEvents
#' @importFrom jsonlite toJSON
create_tsops_graph <- function(nodes,
                               edges,
                               width = 1000,
                               height = 600,
                               main = "Timeseries operator graph",
                               level_separation = 400,
                               node_spacing = 150,
                               tree_spacing = 50,
                               fit_on_stabilize = TRUE) {

  v <- visNetwork::visNetwork(nodes, edges,
                              width = width,
                              height = height,
                              main = main) %>%
    visNetwork::visOptions(
      highlightNearest = TRUE,
      nodesIdSelection = list(enabled = TRUE, main = "Operator"),
      selectedBy = list(variable = "group", main = "Bandpass")
    ) %>%
    visNetwork::visHierarchicalLayout(
      direction = "LR",
      sortMethod = "directed",
      levelSeparation = level_separation,
      nodeSpacing = node_spacing,
      treeSpacing = tree_spacing
    )

  if (fit_on_stabilize) {
    v <- v %>%
      visNetwork::visEvents(
        stabilized = "function() { this.fit({animation: false}); }"
      )
  }

  v
}


#' Create Timeseries Operator Graph with Highlighted Nodes
#'
#' High-level function that creates a complete visNetwork graph from raw data,
#' with optional filtering to show only components containing highlighted nodes.
#'
#' @param tsops_nodes Data frame with columns: tag, bandpass, description
#' @param tsops Data frame with columns: inputtag, branch
#' @param highlight_tags Character vector of node tags to highlight (default: NULL)
#' @param filter_components Logical, filter to only show components containing
#'   highlighted nodes (default: FALSE)
#' @param width Widget width in pixels (default: 1000)
#' @param height Widget height in pixels (default: 600)
#' @param main Plot title (default: "Timeseries operator graph")
#' @param level_separation Horizontal spacing between hierarchy levels (default: 400)
#' @param node_spacing Vertical spacing between nodes (default: 150)
#' @param tree_spacing Spacing between disconnected trees (default: 50)
#' @return visNetwork widget
#' @export
#' @examples
#' \dontrun{
#' # Basic usage
#' plot_tsops_graph(tsops_nodes, tsops)
#'
#' # With highlighting
#' tags <- parse_pg_array(df.tags_active$tags_used[1])
#' plot_tsops_graph(tsops_nodes, tsops, highlight_tags = tags)
#'
#' # Filter to only show relevant components
#' plot_tsops_graph(tsops_nodes, tsops, highlight_tags = tags, filter_components = TRUE)
#' }
plot_tsops_graph <- function(tsops_nodes,
                             tsops,
                             highlight_tags = NULL,
                             filter_components = FALSE,
                             width = 1000,
                             height = 600,
                             main = "Timeseries operator graph (tags used highlighted)",
                             level_separation = 400,
                             node_spacing = 150,
                             tree_spacing = 50) {

  # Create nodes and edges
  nodes <- create_tsops_nodes(tsops_nodes, highlight_tags = highlight_tags)
  edges <- create_tsops_edges(tsops)

  # Optionally filter to highlighted components
  if (filter_components && !is.null(highlight_tags) && length(highlight_tags) > 0) {
    filtered <- filter_graph_components(nodes, edges, highlight_tags)
    nodes <- filtered$nodes
    edges <- filtered$edges
  }

  # Create and return graph
  create_tsops_graph(
    nodes = nodes,
    edges = edges,
    width = width,
    height = height,
    main = main,
    level_separation = level_separation,
    node_spacing = node_spacing,
    tree_spacing = tree_spacing,
    fit_on_stabilize = TRUE
  )
}
