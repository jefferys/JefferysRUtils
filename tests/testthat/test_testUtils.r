describe( "Checking equality including null with expect_equalOrNull", {
	aList <- list(a=1, b=2)
	bList <- list(b=2, a=1)
	it( "Works like expect_equal if 'want' is not null", {
		expect_success(expect_equalOrNull( 1, 1 ))
		expect_success(expect_equalOrNull( aList, aList ))

		expect_failure(expect_equalOrNull( aList, bList ))
		expect_failure(expect_equalOrNull( 1, "1" ))
		expect_failure(expect_equalOrNull( NULL, 1 ))

	})
	it( "Works like is_null if 'want' is null", {
		expect_success( expect_equalOrNull( aList$c, NULL ))
		expect_failure( expect_equalOrNull( aList["c"], NULL ))
		expect_failure(expect_equalOrNull( 1, NULL ))
	})
})
