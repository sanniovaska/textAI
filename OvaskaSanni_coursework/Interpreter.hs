module Interpreter where
  
    import AbsGram

    interpState :: Statement -> String
    interpState x = case x of
        Location per v pla -> "Loc " ++ interpPerson per ++ " " ++ interpPlace pla
        EitherLoc per pla1 pla2 -> "Either " ++ interpPerson per ++ " " ++ interpPlace pla1 ++ " " ++ interpPlace pla2
        NotLoc per pla -> "NotLoc " ++ interpPerson per ++ " " ++ interpPlace pla
        Take per v obj -> "Take " ++ interpPerson per ++ " " ++ interpObject obj
        Drop per v obj -> "Drop " ++ interpPerson per ++ " " ++ interpObject obj
        Hand per1 v obj per2 -> "Hand " ++ interpPerson per1 ++ " " ++ interpPerson per2 ++ " " ++ interpObject obj
        Dir pla1 dir pla2 -> "Dir " ++ interpPlace pla1 ++ " " ++ interpPlace pla2 ++ " " ++ interpDir dir

    interpPerson :: Person -> String
    interpPerson x = case x of
        EPerson p -> interpIdent p

    interpPlace :: Place -> String
    interpPlace x = case x of
        EPlace p -> interpIdent p

    interpObject :: Object -> String
    interpObject x = case x of
        EObject o -> interpIdent o

    interpDir :: EDirection -> String
    interpDir x = case x of
        Dnorth -> "north"
        Deast -> "east"
        Dsouth -> "south"
        Dwest -> "west"

    interpIdent :: Ident -> String
    interpIdent x = case x of
        Ident s -> s
