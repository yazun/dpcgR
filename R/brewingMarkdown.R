
#' Create markdown chunks automatically.
#'
#' Check the code in the notebook as it contains escapes confusing for generated HTML.
#' Data for chunks is created in parallel and chunks are generated based on the created data, then executed in-place with the `child` argument of the chunk.
#' @param listItems list of items to generate chunks on
#' @param fn function to generate data for the listItems
#' @param xm.groups actual data
#' @param xm.skymapData additional data, here the skymap for the function \code{fn}.
#'
#' @return chunks to be included in the markdown via 'child' reference
#' @export
#' @importFrom future.apply future_lapply
#' @importFrom brew brew
#' @importFrom utils capture.output
#' @examples \dontrun{brewed.chunks = brew_plot_chunks(xm.groups_big[1]
#',plotAitoffGalacticOverlayBig
#',xm.groups_big
#',xm.skymap)
#'
#' # include all brewed chunks
#'```{r brewing_big_chunks, child = brewed.chunks}
#'
#'```
#'}
brew_plot_chunks<- function (listItems, fn, xm.groups, xm.skymapData) {

  brewed.chunks <- tempfile(tmpdir = getwd(), pattern = "brewed-chunks_", fileext = ".Rmd")
  # this parallelizes calcualation. But rendering is slow on the current device with plot in subsequent chunks, not this.
  chunksData = future_lapply(xm.groups,
                             function(x)   fn(classGroup =  x, xm.skymap = xm.skymapData ))
  i = 0
  for( x in xm.groups) {
    i=i+1
    items = unlist(x)
    item_names = paste(items,collapse="_")
    section_name = paste(paste(items,collapse=" "),"classes")
    item_chunk_names = paste("chunk_", item_names, sep="")

    # generate chunks in a child file to be executed later.
    capture.output(
      brew( text= quote("
### <%=section_name%>\n\n
```{r <%=item_chunk_names%>, fig.height=10, fig.width=20}\n
chunksData[[<%=i%>]]\n\n
```\n") )
      , file=brewed.chunks, append=TRUE )
  }
  return(brewed.chunks)
}
