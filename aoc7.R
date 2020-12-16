w <- strsplit(readLines("aoc7"), " contain ")
q <- lapply(w, gsub, pattern = " *bags? *", replacement = "")
q <- lapply(q, gsub, pattern = "no other", replacement = "0")
out_bag <- sapply(q, `[[`, 1)
outbag <- gsub("\\d|\\s|\\.", "", out_bag)
in_bag <- sapply(q, `[[`, 2)
inbag <- gsub("\\d|\\s|\\.", "", in_bag)
nbags <- lapply(regmatches(in_bag, gregexpr("\\d+", in_bag)), as.integer)
inbags <- strsplit(inbag, ",")
names(inbags) <- outbag
names(nbags) <- outbag
find_gold <- function(bag) {
  if (any(unlist(inbags[bag]) == "shinygold")) {
    TRUE
  } else {
    (this_bag <- unique(unlist(c(bag, inbags[bag]))))
    if (identical(sort(this_bag), sort(bag))) {
      FALSE
    } else {
      Recall(this_bag)
    }
  }
}
result1 <- sum(sapply(outbag, find_gold))

count_bags <- function(bag) {
  n <- sum(nbags[[bag]])
  for (i in seq_along(inbags[[bag]])) {
    n <- n + nbags[[bag]][[i]] * Recall(inbags[[bag]][[i]])
  }
  n
}

count_bags("shinygold")
