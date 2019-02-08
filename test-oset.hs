import OSet

main1 = let s1 = fromList [4,3,4]
            s2 = insertAll [1,2,3,4] s1
            s3 = fromList [1,5,6,4]
            s4 = intersection s3 s2
            s5 = union s4 . newSet $ 69
            s6 = fromList [1,2,3,4]
            s7 = fromList [1,2,3]
            s8 = fromList [-1,-2,-3,-4]
            o1 = directProd s8 s7
            o2 = directProd s7 s8
       in 
        do  print $ toList s1
            print $ toList s2
            print $ toList s3
            print $ element 5 s3
            print $ element 6 s3
            print $ toList s4
            print $ toList s5
            print . toList $ complement s5 emptyset
            print . toList $ complement emptyset s5
            print . toList $ union s5 emptyset
            print . toList $ union emptyset s5
            print $ isSubset s6 s7
            print $ isSubset s7 s6
            print $ equals s6 s7
            print $ toList o1
            print $ toList o2

main2 = let s1 = fromList [1,2,3]
            s2 = fromList [1,2]
            s3 = fromList [1]
        in do
          print $ isSubset s3 s2
          print $ isSubset s3 emptyset


main = main1
