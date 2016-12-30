package com.programmaticallyspeaking.ncd.infra

import java.util.concurrent.atomic.AtomicInteger

class IdGenerator(prefix: String) {
  private val counter = new AtomicInteger(0)
  def next: String = prefix + counter.incrementAndGet()
}
