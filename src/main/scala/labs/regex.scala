package labs

class regex {
//  1. Natural Numbers
//  A natural numbers is an unsigned zero or a positive integer.
//
//    scala> val natPattern =
//
//  scala> "0".matches(natPattern)
//  res0: Boolean = true
//
//  scala> "007".matches(natPattern)
//  res1: Boolean = false
//
//  scala> "700".matches(natPattern)
//  res2: Boolean = true
//
//  2. Integers
//    A signed integer is zero or a positive integer optionally preceded by a +or – sign.
//
//    scala> val intPattern = "0|(\\+|-)?[1-9][0-9]*" OR """0|(\+|-)?[1-9][0-9]*"""
//
//  scala> "0".matches(intPattern)
//  res3: Boolean = true
//
//  scala> "007".matches(intPattern)
//  res4: Boolean = false
//
//  scala> "100".matches(intPattern)
//  res5: Boolean = true
//
//  scala> "-100".matches(intPattern)
//  res6: Boolean = true
//
//  scala> "+100".matches(intPattern)
//  res7: Boolean = true
//
//  scala> "-0".matches(natPattern)
//  res8: Boolean = false
//
//  3. Floating Point Numbers
//    A floating point number is a signed integer optionally followed by a decimal point and a sequence of digits.
//
//    scala> val floatPattern = "0|(\\+|-)?0\\.[0-9]+|(\\+|-)?[1-9][0-9]*(\\.[0-9]+)?"
//
//  scala> "100".matches(floatPattern)
//  res0: Boolean = true
//
//  scala> "100.001".matches(floatPattern)
//  res1: Boolean = true
//
//  scala> "+100.001".matches(floatPattern)
//  res1: Boolean = true
//
//  scala> "-100.001".matches(floatPattern)
//  res1: Boolean = true
//
//  scala> "100.".matches(floatPattern)
//  res2: Boolean = false
//
//  scala> ".001".matches(floatPattern)
//  res3: Boolean = false
//
//  scala> "0.001".matches(floatPattern)
//  res4: Boolean = true
//
//  scala> "007".matches(floatPattern)
//  res5: Boolean = false
//
//  4. Date Pattern
//  A date has the format month/day/year optionally followed by /CE or /BCE.
//
//  scala> val datePattern = ???
//
//  Two ways to write Jesus' birthday:
//
//    scala> "1/25/1".matches(datePattern)
//  res9: Boolean = true
//
//  scala> "1/25/1/CE".matches(datePattern)
//  res10: Boolean = true
//
//  And the year before:
//
//    scala> "1/25/1/BCE".matches(datePattern)
//  res11: Boolean = true
//
//  There is no year, month, or day 0:
//
//    scala> "0/25/1".matches(datePattern)
//  res12: Boolean = false
//
//  scala> "1/25/0".matches(datePattern)
//  res15: Boolean = false
//
//  scala> "1/0/2018".matches(datePattern)
//  res17: Boolean = false
//
//  The year can be anything from year 1 to year 9999. (We will have a "Year 10000" bug, though.)
//
//  scala> "1/1/2018".matches(datePattern)
//  res16: Boolean = true
//
//  scala> "12/25/800".matches(datePattern)
//  res13: Boolean = true
//
//  scala> "1/1/10000".matches(datePattern)
//  res21: Boolean = false
//
//  A month can be anything from 1 to 12:
//
//    scala> "13/25/800".matches(datePattern)
//  res14: Boolean = false
//
//  The day can be anything from 1 to 31:
//
//    scala> "1/35/2018".matches(datePattern)
//  res18: Boolean = false
//
//  scala> "2/31/2018".matches(datePattern)
//  res19: Boolean = true
//
//  A more complex pattern would not allow this last date.
//
//  5. URL Pattern
//  Here are some examples of URLs:
//
//    http://www.sjsu.edu/cs/index.html
//  www.ibm.com/dev/info.html
//  https://www.whitehouse.gov/trump/crimes.html
//
//    Write a regular expression that matches all URLs of this form.
}
