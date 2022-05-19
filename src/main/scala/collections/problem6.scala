package collections

object problem6 extends App {

  def spellCheck(doc: String, dict: List[String]): Int =
    doc.split("\\s+|\\.\\s?")
      .map(_.toLowerCase())
      .filter(!dict.contains(_))
      .filter(_.matches("[a-zA-Z]+"))
      .length

  val oed = List("dog", "cat", "bat", "bug", "fox", "see", "run", "bite", "the", "a", "and")
  val essay = "See the blue dog run . See the blue dog bite the man ."
  println(spellCheck(essay, oed)) // 3
}
