# Tests for functions in "duplicates.R"

describe( "isDuplicated()", {
   # Relies on error handling provided via the wrapped "duplicated()" functions.
   # SRJ: TODO: Add incomparables= example and test
   describe( "for vector", {
      it( "finds duplicates, including NA's", {
         expect_equal( isDuplicated( c( 1, 1 )),
                       c( TRUE, TRUE ))
         expect_equal( isDuplicated( c( "A", "B", "A" )),
                       c( TRUE, FALSE, TRUE ))
         expect_equal( isDuplicated( c( NA, NA, TRUE, FALSE, NA )),
                       c( TRUE, TRUE, FALSE, FALSE, TRUE ))
      })
      it( "ok if no duplicates, or empty", {
         expect_equal( isDuplicated( c( 1, 2, 3, NA )),
                       c( FALSE, FALSE, FALSE, FALSE ))
         expect_equal( isDuplicated( integer() ), logical() )
      })
   })
   describe( "for data frame", {
      it( "finds duplicates rows, including rows with NA's", {
         df <- data.frame( A=c(  1,   2,   1),
                           B=c("a", "a", "a"))
         expect_equal( isDuplicated( df ), c( TRUE, FALSE, TRUE ))
         df <- data.frame( A=c(  1,   2,   1,  NA, NA, NA),
                           B=c("a", "a", "a", "a", NA, NA))
         expect_equal( isDuplicated( df ), c( TRUE, FALSE, TRUE, FALSE, TRUE, TRUE ))
      })
      it( "ok if no duplicates, or empty", {
         df <- data.frame( A=c(  1,   2,   NA),
                           B=c("a", "a", "a"))
         expect_equal( isDuplicated( df ), c( FALSE, FALSE, FALSE ))
      })
   })
   describe( "for matrix", {
      describe( "default, by row", {
         it( "finds duplicates rows by default, including rows with NA's", {
            mat <- matrix( c( 1, NA, 2,
                              3,  3, 3,
                              1, NA, 2  ), nrow= 3, byrow= TRUE )
            expect_equal( as.vector(isDuplicated( mat )), c( TRUE, FALSE, TRUE ))
         })
         it( "ok if no duplicates", {
            mat <- matrix( c( 1, NA, 2,
                              3,  3, 3,
                              1,  2, NA ), nrow= 3, byrow= TRUE )
            expect_equal( as.vector(isDuplicated( mat )), c( FALSE, FALSE, FALSE ))
         })
         # Note - Ignoring degenerate matrices/arrays!
      })
      describe( "explicit, by colun", {
         it( "finds duplicates columns with MARGIN=2, including cols with NA's", {
            mat <- matrix( c( 1, NA, 2,
                              3,  3, 3,
                              1, NA, 2  ), nrow= 3, byrow= FALSE )
            expect_equal( as.vector(isDuplicated( mat, MARGIN= 2 )), c( TRUE, FALSE, TRUE ))
         })
         it( "ok if no duplicates", {
            mat <- matrix( c( 1, NA, 2,
                              3,  3, 3,
                              1,  2, NA ), nrow= 3, byrow= FALSE )
            expect_equal( as.vector(isDuplicated( mat, MARGIN= 2 )), c( FALSE, FALSE, FALSE ))
         })
         # Note - Ignoring degenerate matrices/arrays!
      })
   })
})
