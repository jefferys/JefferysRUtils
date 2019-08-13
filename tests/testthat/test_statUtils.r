context( "Testing statUtils.R" )

describe( "cdfDiff()", {
   describe( "parameter validation", {
      it( "Fails if any required parameter is missing", {
         wantRE <- paste0( "^cdfDiff\\(\\) requires at least 3 parameters; ",
                       "'a', 'b', and 'dist'\\.$" )
         expect_error( cdfDiff(), wantRE )
         expect_error( cdfDiff( 1 ), wantRE )
         expect_error( cdfDiff( 1, 1.1 ), wantRE )
         expect_error( cdfDiff(    b= 1 ), wantRE )
         expect_error( cdfDiff(         dist= norm ), wantRE )
         expect_error( cdfDiff( 1,      dist= norm ), wantRE )
         expect_error( cdfDiff(    b= 1, dist= norm ), wantRE )

      })
      it( "Fails if a and b are neither equal length nor either length 1", {
         wantRE= paste0( "^cdfDiff\\(\\) parameters 'a' and 'b' must be the ",
                         "same length when neither is length 1\\.$" )
         a <- c(0, 1)
         b <- c(0.1, 1.1, 2)
         expect_error( cdfDiff(a, b, "norm"), wantRE )
      })
      it( "Fails if a or b is empty", {
         wantRE= "^cdfDiff\\(\\) parameter 'a' may not be empty\\.$"
         expect_error( cdfDiff( integer(0), 1, "norm"), wantRE )

         wantRE= "^cdfDiff\\(\\) parameter 'b' may not be empty\\.$"
         expect_error( cdfDiff( 1, integer(0), "norm"), wantRE )
      })
      it( "Fails if dist is unknown", {
         wantRE= paste0( "^Specified 'dist' parameter for cdfDiff\\(\\) is ",
            "unknown or unsupported: \"noSuchDist\"\\.$" )
         expect_error( cdfDiff( 1, 1, "noSuchDist"), wantRE )
      })
   })
   describe( "the a and b parameters give the rage to find the probability of", {
      it( "for scalar values and default distribution", {
         expect_equal( cdfDiff( -Inf, Inf, "norm" ), 1 )
         expect_equal( cdfDiff( Inf, -Inf, "norm" ), -1 )
         expect_equal( cdfDiff( -Inf, 0, "norm" ), 0.5 )
         expect_equal( cdfDiff( 0, Inf, "norm" ), 0.5 )
         expect_equal( cdfDiff( -0.2, -0.2, "norm" ), 0 )
      })
      it( "for scalar values and different distribution", {
         expect_equal( cdfDiff( -Inf, 2, "norm", mean=2 ), 0.5 )
      })
      it( "for 1 scalar and one vector value, default distributions", {
         expect_equal( cdfDiff( -Inf, c(-Inf, 0, Inf), "norm" ), c(0, 0.5, 1) )
         expect_equal( cdfDiff( c(-Inf, 0, Inf), Inf, "norm" ), c(1, 0.5, 0) )
      })
      it( "for 1 scalar and one vector value, different distributions", {
         expect_equal( cdfDiff( -Inf, c(-Inf, -1, Inf), "norm", mean= -1 ),
                       c(0, 0.5, 1) )
         expect_equal( cdfDiff( c(-Inf, 1, Inf), Inf, "norm", mean= 1 ),
                       c(1, 0.5, 0) )
      })
      it( "for vector values, default distributions", {
         expect_equal( cdfDiff( c(-Inf, 0, Inf), c(0, Inf, Inf), "norm" ),
                       c(0.5, 0.5, 0) )
      })
      it( "for vector values, different distributions", {
         expect_equal( cdfDiff( c(2.1, -Inf, Inf), c(Inf, 2.1, Inf),
                                "norm", mean= 2.1 ),
                       c(0.5, 0.5, 0) )
      })
   })
   describe( "the dist parameter", {
      it( "is not case sensitive", {
         expect_equal( cdfDiff( -0.2, -0.2, "Norm" ), 0 )
         expect_equal( cdfDiff( -0.2, -0.2, "NORM" ), 0 )
         expect_equal( cdfDiff( -0.2, -0.2, "nOrM" ), 0 )
      })
      it( "matches on partial distribution names", {
         expect_equal( cdfDiff( -0.2, -0.2, "norm" ), 0 )
         expect_equal( cdfDiff( -0.2, -0.2, "norma" ), 0 )
         expect_equal( cdfDiff( -0.2, -0.2, "nor" ), 0 )
      })
   })
})
