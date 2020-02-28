package example

class ExampleSpec extends TestSpec {
  "42" should "=== 42" in {
    42 should ===(42)
  }
}
