context( "Simple string utils" )

describe( "Binary paste operators %p% and %pp%", {
   it( "Joins without spaces using %p%.", {
      expect_equal( "a" %p% "b", "ab")
      expect_equal( "a " %p% " b", "a  b")
      expect_equal( c("a", "b") %p% c(1, 2), c("a1", "b2"))
   })
   it( "Joins with spaces using %pp%.", {
      expect_equal( "a" %pp% "b", "a b")
      expect_equal( "a " %pp% " b", "a   b")
      expect_equal( c("a", "b") %pp% c(1, 2), c("a 1", "b 2"))
   })
   it( "Joins using content of variables", {
      aVar <- 123
      expect_equal( "a" %p% "b" %p% "c" %p% aVar, "abc123")
      expect_equal( "a" %pp% "b" %pp% "c" %pp% aVar, "a b c 123")
      expect_equal( "a" %p% "b" %pp% "c" %p% aVar %pp% 4, "ab c123 4")
   })
})
