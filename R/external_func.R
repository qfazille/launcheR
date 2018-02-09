#' @rdname createQueue
#' @title createQueue
#' @description Create an empty queue.
#' @param name Character Alias name of the queue. (Default basename(tempfile(pattern = "file", tmpdir = "")))
#' @param group Character Name of the group the queue belongs to. (Default NULL)
#' @param folder Character Path to a folder that will contains the working directory folder. (Default tempdir())
#' @param logdir Character Path to the folder that will contains logs file. (By default in folder)
#' @param clean Logical Whether or not the working directory folder should be removed. (Default TRUE)
#' @param tmpdir Character If use through RSConnect then you must define a tmpdir not in /tmp/*. (Default TRUE)
#' @export
#' @examples 
#' \dontrun{
#' q <- createQueue()
#' }
#' @importFrom methods new
createQueue <- function(name = basename(tempfile(pattern = "queue", tmpdir = "")), group = NULL, folder = tempdir(), logdir = NULL, clean = TRUE, tmpdir = NULL) {
    new(Class = "queue", name = name, group = group, folder = folder, logdir = logdir, clean = clean, tmpdir = tmpdir)
}


