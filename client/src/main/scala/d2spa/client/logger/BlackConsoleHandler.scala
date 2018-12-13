package java.util.logging

import java.util.logging.{LogRecord, SimpleFormatter, StreamHandler}

class BlackConsoleHandler extends ConsoleHandler {
  setOutputStream(System.out)
}
