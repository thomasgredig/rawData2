# Divert all messages to a null device (silences them completely)
sink_file <- file(nullfile(), open = "wb")
sink(sink_file, type = "message")

# Ensure the diversion is closed when tests finish
withr::defer(
  {
    sink(type = "message")
    close(sink_file)
  },
  teardown_env()
)

