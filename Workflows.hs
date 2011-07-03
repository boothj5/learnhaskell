data Person = Person { firstName  :: String
                     , secondName :: String
                     , age        :: Int }

instance Show Person where
    show p = concat [ firstName p
                    , " "
                    , secondName p
                    , ", "
                    , show (age p) ]

data EpisodeType = Contact | Referral | Assessment
    deriving Show

data Outcome = NFA 
             | Outcome { nextEpisode :: Episode }

instance Show Outcome where
    show NFA = "NFA"
    show o   = title . nextEpisode $ o

data Episode = Episode { title    :: String
                       , eType    :: EpisodeType
                       , subject  :: Person
                       , comments :: String
                       , outcomes :: [Outcome] }

instance Show Episode where
    show e = concat [ "TITLE:    ", title e, "\n"
                    , "SUBJECT:  ", show (subject e), "\n"
                    , "COMMENTS: ", comments e, "\n"
                    , "OUTCOMES:\n"
                    , concat (map showTabbed (outcomes e)) ]
        where showTabbed o = "--> " ++ show o ++ "\n"
    

newtype EpisodeStore = EpisodeStore { getEpisodes :: [Episode] }

instance Show EpisodeStore where
    show store | null (getEpisodes store) 
                        = "End of Episode list"
               | otherwise                
                        = show (head (getEpisodes store)) ++ 
                          "\n" ++ show (EpisodeStore { getEpisodes = tail (getEpisodes store)})

james = Person { firstName  = "James"
               , secondName = "Booth"
               , age        = 34 }

joe = Person { firstName  = "Joe"
             , secondName = "Smith"
             , age        = 31 }


contact = Episode { title    = "Initial Contact"
                  , eType    = Contact
                  , subject  = james
                  , comments = "This was the initial contact"
                  , outcomes = [ Outcome { nextEpisode = referral1 }
                               , Outcome { nextEpisode = referral2 } ] }

referral1 = Episode { title    = "Referral 1"
                    , eType    = Referral
                    , subject  = james
                    , comments = "This referral needs no more work"
                    , outcomes = [ NFA ] }

 
referral2 = Episode { title = "Referral 2"
                    , eType = Referral
                    , subject = james
                    , comments = "This is the main referral"
                    , outcomes = [ Outcome { nextEpisode = assessment } ] }

assessment = Episode { title    = "Assessment"
                     , eType    = Assessment
                     , subject  = james
                     , comments = "This is the assessment episode"
                     , outcomes = [ NFA ] }

joeContact = Episode { title    = "Joe contact"
                     , eType    = Contact
                     , subject  = joe
                     , comments = "No problems"
                     , outcomes = [ NFA ] }



newEpisodeStore :: EpisodeStore
newEpisodeStore = EpisodeStore { getEpisodes = [] }

addEpisode :: Episode -> EpisodeStore -> EpisodeStore
addEpisode e store = EpisodeStore { getEpisodes = e:(getEpisodes store) }

episodes = addEpisode joeContact 
            (addEpisode assessment 
            (addEpisode referral2 
            (addEpisode referral1 
            (addEpisode contact newEpisodeStore))))

