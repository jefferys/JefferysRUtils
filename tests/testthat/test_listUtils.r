context("Testing list utils")

describe( "merge() is extended to merge lists when everything has a name", {
   describe( "preconditions on lists being merged and the names of their elements", {
      it( "Errors when first element is not a list, if call explicitly", {
         listAB <- list(A=1, B=2, D=4)
         notList <- 'x'
         errorRE = "Can't merge lists; 'x' is not a list\\."
         expect_error( merge.list(notList, listAB), errorRE )
      })
      it( "Errors when second element is not a list", {
         listAB <- list(A=1, B=2, D=4)
         notList <- 'x'
         errorRE = "Can't merge lists; 'y' is not a list\\."
         expect_error( merge(listAB, notList ), errorRE )
      })
      it( "Errors when either list has unnamed elements", {
         allNamedList <- list(A=1, A=2)
         missingNamesList <- list(A=1,2)
         errorRE = "'x' contains elements without names\\."
         expect_error( merge(missingNamesList, allNamedList), errorRE )
         errorRE = "'y' contains elements without names\\."
         expect_error( merge(allNamedList, missingNamesList), errorRE )
      })
      it( "Errors when either list has duplicated element names", {
         duplicatedNamesList <- list(A=1, A=2)
         uniqueNamesList <- list(A=1, B=2)
         errorRE = "'x' contains elements with duplicated names\\."
         expect_error( merge(duplicatedNamesList, uniqueNamesList), errorRE )
         errorRE = "'y' contains elements with duplicated names\\."
         expect_error( merge(uniqueNamesList, duplicatedNamesList), errorRE )
      })
   })
	describe( "merging lists by appending by default", {
		it( "Merging when list have unique names", {
			listA <- list(A1=1, A2=2)
			listB <- list(B1=3, B2=4)
			want <- list(A1=1, A2=2, B1=3, B2=4)
			got <- merge(listA, listB)
			expect_equal(got, want)
		})
      it( "When the same name is present in both lists, by default the element from \
           the first list is dropped before appending the second", {
         listAB <- list(A=1, B=2, D=4)
         listBC <- list(B="b", C="c", A="a")
         want <- list(D=4, B="b", C="c", A="a")
         got <- merge(listAB, listBC)
         expect_equal(got, want)
      })
      it( "Works with empty lists", {
      	listA <- list(A=1, B=2)
      	listEmpty <- list()

      	want <- listA
      	got <- merge(listA, listEmpty)
      	expect_equal(got, want)
      	got <- merge(listEmpty, listA)
      	expect_equal(got, want)

      	want <- listEmpty
      	got <- merge(listEmpty, listEmpty)
      	expect_equal(got, want)
      })
	})
   describe( "merging lists keeps order with keepOrder set", {
   	it( "Keeps order when list have unique names", {
   		listA <- list(A1=1, A2=2)
   		listB <- list(B1=3, B2=4)
   		want <- list(A1=1, A2=2, B1=3, B2=4)
   		got <- merge(listA, listB, keepOrder=TRUE)
   		expect_equal(got, want)
   	})
      it( "If the user wants to retain the order of elements as musch as possible, \
          duplicate names in the first list have their values replaced and these \
          are dropped from the second list when appended.", {
         listAB <- list(A=1, B=2, D=4)
         listBC <- list(B="b", C="c", A="a")
         want <- list(A="a", B="b", D=4, C="c" )
         got <- merge(listAB, listBC, keepOrder=TRUE)
         expect_equal(got, want)
      })
      it( "keepOrder works fine with empty lists too", {
      	listA <- list(A=1, B=2)
      	listEmpty <- list()

      	want <- listA
      	got <- merge(listA, listEmpty, keepOrder=TRUE)
      	expect_equal(got, want)
      	got <- merge(listEmpty, listA, keepOrder=TRUE)
      	expect_equal(got, want)

      	want <- listEmpty
      	got <- merge(listEmpty, listEmpty, keepOrder=TRUE)
      	expect_equal(got, want)
      })
   })
})

