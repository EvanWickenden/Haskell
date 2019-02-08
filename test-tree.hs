
import Tree

main1 = let r = fromList ([9,8,7,6,5,2,8,9,1,2,1] :: [Int])
            r' = addOnce 1 $ fromList ([1] :: [Int])
            (r'',b) = removed 9 r
            (r''',b') = removed 9 r''
            (s,b'') = removed 9 r'''
        in do
          print $ toList r
          print $ isBalanced r
          print $ toList r'
          print $ isBalanced r'
          print $ toList r''
          print $ b
          print $ isBalanced r''
          print $ toList r'''
          print $ b'
          print $ isBalanced r'''
          print $ toList s
          print $ b''
          print $ isBalanced s

main2 = let r = fromList ([1] :: [Int])
            (r',b) = removed 2 r
            (r'',b') = removed 1 r'
            (r''',b'') = removed 2 r''
        in do
          print b
          print b'
          print b''

main = main2
