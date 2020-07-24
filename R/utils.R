library(tidyverse)

#' Convert a duration in milliseconds to a frame count
#' @param ms milliseconds
#' @param fps frame rate
#' @param floor whether to return the answer as maximum number of frames in the duration (T) or exact answer including partial frames (F)
#' @return frames in \code{ms}
ms_to_frames <- function(ms, fps = 30, floor = T) {
  f <- fps * ms / 1000
  if (floor)
    floor(f)
  else
    f
}

#' Find the millisecond at which frames begin
#' @param frameNumbers frames to get the beginnings of
frames_to_ms <- function(frameNumbers, fps = 30) {
  frameNumbers / fps * 1000
}


