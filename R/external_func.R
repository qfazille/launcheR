#' @export
createQueue <- function(name = basename(tempfile(pattern = "queue", tmpdir = "")), group = NULL, folder = tempdir(), logdir = NULL, clean = TRUE) {
    new(Class = "queue", name = name, group = group, folder = folder, logdir = logdir, clean = clean)
}


