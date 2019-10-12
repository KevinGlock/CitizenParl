## Sorted dictionary adapted from Green and theoretical framework (LDA cross-validated)

dict <-read.csv(file= "C:/Users/admin/dict_citizenship2.csv", header=TRUE, sep = ";")

View(dict)

##                        actors                                                         issues
## 1            [Dd]oppelstaat.*                                              .*[Aa]us.bürger.*
## 2              [Mm]ehrstaat.*                                              .*[Ee]in.bürger.*
## 3         .*[Ss]taatsbürger.*                                               Doppelpa(ss|ß).*
## 4 .*[Ss]taats(an|zu)gehörig.* '"[Dd]oppelt.*" [] "[Ss]taat.*"', '"[Dd]oppel.*" "[Ss]taat.*"'
## 5             [Ss]taatenlos.*                                                       Pa(ss|ß)
## 6                                                                        [Oo]ptionspflicht.*
## 7                                                                          [Oo]ptionszwang.*
##                                                           norm
## 1                                                 Blutsrecht.*
## 2                                     Geburts(recht|prinzip).*
## 3 '"[Ii]us" "(soli|sanguinis)"', '"[Jj]us" "(soli|sanguinis)"'
## 4                                 Abstammungs(recht|prinzip).*