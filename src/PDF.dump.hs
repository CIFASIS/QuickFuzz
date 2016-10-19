GHCi, version 7.10.3: http://www.haskell.org/ghc/  :? for help
*** WARNING: /home/agustin/.ghci is writable by someone else, IGNORING!
Suggested fix: execute 'chmod go-w /home/agustin/.ghci'
[1 of 8] Compiling ByteString       ( ByteString.hs, interpreted )
[2 of 8] Compiling ProbGen          ( ProbGen.hs, interpreted )
[3 of 8] Compiling Vector           ( Vector.hs, interpreted )
[4 of 8] Compiling Strings          ( Strings.hs, interpreted )
[5 of 8] Compiling DeriveShow       ( DeriveShow.hs, interpreted )
[6 of 8] Compiling DeriveArbitrary  ( DeriveArbitrary.hs, interpreted )
[7 of 8] Compiling DeriveMArbitrary ( DeriveMArbitrary.hs, interpreted )
[8 of 8] Compiling PDF              ( PDF.hs, interpreted )
PDF.hs:21:3-44: Splicing declarations
    devMArbitrary "Graphics.EasyRender" ''Draw
  ======>
    data DrawAction
      = Act_Draw_arc X Y Double Double Double |
        Act_Draw_arc_append X Y Double Double Double |
        Act_Draw_clip |
        Act_Draw_closepath |
        Act_Draw_comment String |
        Act_Draw_curveto X Y X Y X Y |
        Act_Draw_draw_subroutine [CustomDef] [DrawAction] |
        Act_Draw_endpage X Y |
        Act_Draw_fill Color |
        Act_Draw_fillstroke Color |
        Act_Draw_lineto X Y |
        Act_Draw_moveto X Y |
        Act_Draw_newpath |
        Act_Draw_oval X Y X Y |
        Act_Draw_rectangle X Y X Y |
        Act_Draw_scale X Y |
        Act_Draw_setcolor Color |
        Act_Draw_setlinewidth Double |
        Act_Draw_stroke |
        Act_Draw_textbox Alignment Font Color X Y X Y Double String |
        Act_Draw_translate X Y
    performDraw [] = return GHC.Tuple.()
    performDraw ((Act_Draw_arc v1 v2 v3 v4 v5) : moreDrawActions)
      = (Graphics.EasyRender.Internal.arc v1 v2 v3 v4 v5)
        >> performDraw moreDrawActions
    performDraw
      ((Act_Draw_arc_append v1 v2 v3 v4 v5) : moreDrawActions)
      = (Graphics.EasyRender.Internal.arc_append v1 v2 v3 v4 v5)
        >> performDraw moreDrawActions
    performDraw (Act_Draw_clip : moreDrawActions)
      = Graphics.EasyRender.Internal.clip >> performDraw moreDrawActions
    performDraw (Act_Draw_closepath : moreDrawActions)
      = Graphics.EasyRender.Internal.closepath
        >> performDraw moreDrawActions
    performDraw ((Act_Draw_comment v1) : moreDrawActions)
      = (Graphics.EasyRender.Internal.comment v1)
        >> performDraw moreDrawActions
    performDraw
      ((Act_Draw_curveto v1 v2 v3 v4 v5 v6) : moreDrawActions)
      = (Graphics.EasyRender.Internal.curveto v1 v2 v3 v4 v5 v6)
        >> performDraw moreDrawActions
    performDraw ((Act_Draw_draw_subroutine v1 v2) : moreDrawActions)
      = (Graphics.EasyRender.Internal.draw_subroutine
           v1 (performDraw v2))
        >> performDraw moreDrawActions
    performDraw ((Act_Draw_endpage v1 v2) : moreDrawActions)
      = (Graphics.EasyRender.Internal.endpage v1 v2)
        >> performDraw moreDrawActions
    performDraw ((Act_Draw_fill v1) : moreDrawActions)
      = (Graphics.EasyRender.Internal.fill v1)
        >> performDraw moreDrawActions
    performDraw ((Act_Draw_fillstroke v1) : moreDrawActions)
      = (Graphics.EasyRender.Internal.fillstroke v1)
        >> performDraw moreDrawActions
    performDraw ((Act_Draw_lineto v1 v2) : moreDrawActions)
      = (Graphics.EasyRender.Internal.lineto v1 v2)
        >> performDraw moreDrawActions
    performDraw ((Act_Draw_moveto v1 v2) : moreDrawActions)
      = (Graphics.EasyRender.Internal.moveto v1 v2)
        >> performDraw moreDrawActions
    performDraw (Act_Draw_newpath : moreDrawActions)
      = Graphics.EasyRender.Internal.newpath
        >> performDraw moreDrawActions
    performDraw ((Act_Draw_oval v1 v2 v3 v4) : moreDrawActions)
      = (Graphics.EasyRender.Internal.oval v1 v2 v3 v4)
        >> performDraw moreDrawActions
    performDraw ((Act_Draw_rectangle v1 v2 v3 v4) : moreDrawActions)
      = (Graphics.EasyRender.Internal.rectangle v1 v2 v3 v4)
        >> performDraw moreDrawActions
    performDraw ((Act_Draw_scale v1 v2) : moreDrawActions)
      = (Graphics.EasyRender.Internal.scale v1 v2)
        >> performDraw moreDrawActions
    performDraw ((Act_Draw_setcolor v1) : moreDrawActions)
      = (Graphics.EasyRender.Internal.setcolor v1)
        >> performDraw moreDrawActions
    performDraw ((Act_Draw_setlinewidth v1) : moreDrawActions)
      = (Graphics.EasyRender.Internal.setlinewidth v1)
        >> performDraw moreDrawActions
    performDraw (Act_Draw_stroke : moreDrawActions)
      = Graphics.EasyRender.Internal.stroke
        >> performDraw moreDrawActions
    performDraw
      ((Act_Draw_textbox v1 v2 v3 v4 v5 v6 v7 v8 v9) : moreDrawActions)
      = (Graphics.EasyRender.Internal.textbox v1 v2 v3 v4 v5 v6 v7 v8 v9)
        >> performDraw moreDrawActions
    performDraw ((Act_Draw_translate v1 v2) : moreDrawActions)
      = (Graphics.EasyRender.Internal.translate v1 v2)
        >> performDraw moreDrawActions
"PreVisiting:PDF.DrawAction"
"Visiting: TyConI (DataD [] PDF.DrawAction [] [NormalC PDF.Act_Draw_arc [(NotStrict,ConT Graphics.EasyRender.Internal.X),(NotStrict,ConT Graphics.EasyRender.Internal.Y),(NotStrict,ConT GHC.Types.Double),(NotStrict,ConT GHC.Types.Double),(NotStrict,ConT GHC.Types.Double)],NormalC PDF.Act_Draw_arc_append [(NotStrict,ConT Graphics.EasyRender.Internal.X),(NotStrict,ConT Graphics.EasyRender.Internal.Y),(NotStrict,ConT GHC.Types.Double),(NotStrict,ConT GHC.Types.Double),(NotStrict,ConT GHC.Types.Double)],NormalC PDF.Act_Draw_clip [],NormalC PDF.Act_Draw_closepath [],NormalC PDF.Act_Draw_comment [(NotStrict,ConT GHC.Base.String)],NormalC PDF.Act_Draw_curveto [(NotStrict,ConT Graphics.EasyRender.Internal.X),(NotStrict,ConT Graphics.EasyRender.Internal.Y),(NotStrict,ConT Graphics.EasyRender.Internal.X),(NotStrict,ConT Graphics.EasyRender.Internal.Y),(NotStrict,ConT Graphics.EasyRender.Internal.X),(NotStrict,ConT Graphics.EasyRender.Internal.Y)],NormalC PDF.Act_Draw_draw_subroutine [(NotStrict,AppT ListT (ConT Graphics.EasyRender.Internal.CustomDef)),(NotStrict,AppT ListT (ConT PDF.DrawAction))],NormalC PDF.Act_Draw_endpage [(NotStrict,ConT Graphics.EasyRender.Internal.X),(NotStrict,ConT Graphics.EasyRender.Internal.Y)],NormalC PDF.Act_Draw_fill [(NotStrict,ConT Graphics.EasyRender.Internal.Color)],NormalC PDF.Act_Draw_fillstroke [(NotStrict,ConT Graphics.EasyRender.Internal.Color)],NormalC PDF.Act_Draw_lineto [(NotStrict,ConT Graphics.EasyRender.Internal.X),(NotStrict,ConT Graphics.EasyRender.Internal.Y)],NormalC PDF.Act_Draw_moveto [(NotStrict,ConT Graphics.EasyRender.Internal.X),(NotStrict,ConT Graphics.EasyRender.Internal.Y)],NormalC PDF.Act_Draw_newpath [],NormalC PDF.Act_Draw_oval [(NotStrict,ConT Graphics.EasyRender.Internal.X),(NotStrict,ConT Graphics.EasyRender.Internal.Y),(NotStrict,ConT Graphics.EasyRender.Internal.X),(NotStrict,ConT Graphics.EasyRender.Internal.Y)],NormalC PDF.Act_Draw_rectangle [(NotStrict,ConT Graphics.EasyRender.Internal.X),(NotStrict,ConT Graphics.EasyRender.Internal.Y),(NotStrict,ConT Graphics.EasyRender.Internal.X),(NotStrict,ConT Graphics.EasyRender.Internal.Y)],NormalC PDF.Act_Draw_scale [(NotStrict,ConT Graphics.EasyRender.Internal.X),(NotStrict,ConT Graphics.EasyRender.Internal.Y)],NormalC PDF.Act_Draw_setcolor [(NotStrict,ConT Graphics.EasyRender.Internal.Color)],NormalC PDF.Act_Draw_setlinewidth [(NotStrict,ConT GHC.Types.Double)],NormalC PDF.Act_Draw_stroke [],NormalC PDF.Act_Draw_textbox [(NotStrict,ConT Graphics.EasyRender.Internal.Alignment),(NotStrict,ConT Graphics.EasyRender.Internal.Font),(NotStrict,ConT Graphics.EasyRender.Internal.Color),(NotStrict,ConT Graphics.EasyRender.Internal.X),(NotStrict,ConT Graphics.EasyRender.Internal.Y),(NotStrict,ConT Graphics.EasyRender.Internal.X),(NotStrict,ConT Graphics.EasyRender.Internal.Y),(NotStrict,ConT GHC.Types.Double),(NotStrict,ConT GHC.Base.String)],NormalC PDF.Act_Draw_translate [(NotStrict,ConT Graphics.EasyRender.Internal.X),(NotStrict,ConT Graphics.EasyRender.Internal.Y)]] [])"
"PreVisiting:Graphics.EasyRender.Internal.X"
"Visiting: TyConI (TySynD Graphics.EasyRender.Internal.X [] (ConT GHC.Types.Double))"
"PreVisiting:Graphics.EasyRender.Internal.Y"
"Visiting: TyConI (TySynD Graphics.EasyRender.Internal.Y [] (ConT GHC.Types.Double))"
"PreVisiting:Graphics.EasyRender.Internal.CustomDef"
"Visiting: TyConI (DataD [] Graphics.EasyRender.Internal.CustomDef [] [NormalC Graphics.EasyRender.Internal.CustomDef [(NotStrict,ConT Graphics.EasyRender.Internal.Language),(NotStrict,ConT GHC.Base.String)]] [])"
"PreVisiting:Graphics.EasyRender.Internal.Language"
"Visiting: TyConI (DataD [] Graphics.EasyRender.Internal.Language [] [NormalC Graphics.EasyRender.Internal.Language_PS [],NormalC Graphics.EasyRender.Internal.Language_PDF [],NormalC Graphics.EasyRender.Internal.Language_ASCII []] [])"
"PreVisiting:Graphics.EasyRender.Internal.Color"
"Visiting: TyConI (DataD [] Graphics.EasyRender.Internal.Color [] [NormalC Graphics.EasyRender.Internal.Color_RGB [(NotStrict,ConT GHC.Types.Double),(NotStrict,ConT GHC.Types.Double),(NotStrict,ConT GHC.Types.Double)],NormalC Graphics.EasyRender.Internal.Color_Gray [(NotStrict,ConT GHC.Types.Double)]] [])"
"PreVisiting:Graphics.EasyRender.Internal.Alignment"
"Visiting: TyConI (TySynD Graphics.EasyRender.Internal.Alignment [] (ConT GHC.Types.Double))"
"PreVisiting:Graphics.EasyRender.Internal.Font"
"Visiting: TyConI (DataD [] Graphics.EasyRender.Internal.Font [] [NormalC Graphics.EasyRender.Internal.Font [(NotStrict,ConT Graphics.EasyRender.Internal.Basefont),(NotStrict,ConT GHC.Types.Double)]] [])"
"PreVisiting:Graphics.EasyRender.Internal.Basefont"
"Visiting: TyConI (DataD [] Graphics.EasyRender.Internal.Basefont [] [NormalC Graphics.EasyRender.Internal.TimesRoman [],NormalC Graphics.EasyRender.Internal.Helvetica []] [])"
"Deriving:TyConI (DataD [] Graphics.EasyRender.Internal.Basefont [] [NormalC Graphics.EasyRender.Internal.TimesRoman [],NormalC Graphics.EasyRender.Internal.Helvetica []] [])"
"Deriving:TyConI (DataD [] Graphics.EasyRender.Internal.Color [] [NormalC Graphics.EasyRender.Internal.Color_RGB [(NotStrict,ConT GHC.Types.Double),(NotStrict,ConT GHC.Types.Double),(NotStrict,ConT GHC.Types.Double)],NormalC Graphics.EasyRender.Internal.Color_Gray [(NotStrict,ConT GHC.Types.Double)]] [])"
"Deriving:TyConI (DataD [] Graphics.EasyRender.Internal.Language [] [NormalC Graphics.EasyRender.Internal.Language_PS [],NormalC Graphics.EasyRender.Internal.Language_PDF [],NormalC Graphics.EasyRender.Internal.Language_ASCII []] [])"
"Deriving:TyConI (DataD [] Graphics.EasyRender.Internal.CustomDef [] [NormalC Graphics.EasyRender.Internal.CustomDef [(NotStrict,ConT Graphics.EasyRender.Internal.Language),(NotStrict,ConT GHC.Base.String)]] [])"
"Deriving:TyConI (DataD [] Graphics.EasyRender.Internal.Font [] [NormalC Graphics.EasyRender.Internal.Font [(NotStrict,ConT Graphics.EasyRender.Internal.Basefont),(NotStrict,ConT GHC.Types.Double)]] [])"
"Deriving:TyConI (DataD [] PDF.DrawAction [] [NormalC PDF.Act_Draw_arc [(NotStrict,ConT Graphics.EasyRender.Internal.X),(NotStrict,ConT Graphics.EasyRender.Internal.Y),(NotStrict,ConT GHC.Types.Double),(NotStrict,ConT GHC.Types.Double),(NotStrict,ConT GHC.Types.Double)],NormalC PDF.Act_Draw_arc_append [(NotStrict,ConT Graphics.EasyRender.Internal.X),(NotStrict,ConT Graphics.EasyRender.Internal.Y),(NotStrict,ConT GHC.Types.Double),(NotStrict,ConT GHC.Types.Double),(NotStrict,ConT GHC.Types.Double)],NormalC PDF.Act_Draw_clip [],NormalC PDF.Act_Draw_closepath [],NormalC PDF.Act_Draw_comment [(NotStrict,ConT GHC.Base.String)],NormalC PDF.Act_Draw_curveto [(NotStrict,ConT Graphics.EasyRender.Internal.X),(NotStrict,ConT Graphics.EasyRender.Internal.Y),(NotStrict,ConT Graphics.EasyRender.Internal.X),(NotStrict,ConT Graphics.EasyRender.Internal.Y),(NotStrict,ConT Graphics.EasyRender.Internal.X),(NotStrict,ConT Graphics.EasyRender.Internal.Y)],NormalC PDF.Act_Draw_draw_subroutine [(NotStrict,AppT ListT (ConT Graphics.EasyRender.Internal.CustomDef)),(NotStrict,AppT ListT (ConT PDF.DrawAction))],NormalC PDF.Act_Draw_endpage [(NotStrict,ConT Graphics.EasyRender.Internal.X),(NotStrict,ConT Graphics.EasyRender.Internal.Y)],NormalC PDF.Act_Draw_fill [(NotStrict,ConT Graphics.EasyRender.Internal.Color)],NormalC PDF.Act_Draw_fillstroke [(NotStrict,ConT Graphics.EasyRender.Internal.Color)],NormalC PDF.Act_Draw_lineto [(NotStrict,ConT Graphics.EasyRender.Internal.X),(NotStrict,ConT Graphics.EasyRender.Internal.Y)],NormalC PDF.Act_Draw_moveto [(NotStrict,ConT Graphics.EasyRender.Internal.X),(NotStrict,ConT Graphics.EasyRender.Internal.Y)],NormalC PDF.Act_Draw_newpath [],NormalC PDF.Act_Draw_oval [(NotStrict,ConT Graphics.EasyRender.Internal.X),(NotStrict,ConT Graphics.EasyRender.Internal.Y),(NotStrict,ConT Graphics.EasyRender.Internal.X),(NotStrict,ConT Graphics.EasyRender.Internal.Y)],NormalC PDF.Act_Draw_rectangle [(NotStrict,ConT Graphics.EasyRender.Internal.X),(NotStrict,ConT Graphics.EasyRender.Internal.Y),(NotStrict,ConT Graphics.EasyRender.Internal.X),(NotStrict,ConT Graphics.EasyRender.Internal.Y)],NormalC PDF.Act_Draw_scale [(NotStrict,ConT Graphics.EasyRender.Internal.X),(NotStrict,ConT Graphics.EasyRender.Internal.Y)],NormalC PDF.Act_Draw_setcolor [(NotStrict,ConT Graphics.EasyRender.Internal.Color)],NormalC PDF.Act_Draw_setlinewidth [(NotStrict,ConT GHC.Types.Double)],NormalC PDF.Act_Draw_stroke [],NormalC PDF.Act_Draw_textbox [(NotStrict,ConT Graphics.EasyRender.Internal.Alignment),(NotStrict,ConT Graphics.EasyRender.Internal.Font),(NotStrict,ConT Graphics.EasyRender.Internal.Color),(NotStrict,ConT Graphics.EasyRender.Internal.X),(NotStrict,ConT Graphics.EasyRender.Internal.Y),(NotStrict,ConT Graphics.EasyRender.Internal.X),(NotStrict,ConT Graphics.EasyRender.Internal.Y),(NotStrict,ConT GHC.Types.Double),(NotStrict,ConT GHC.Base.String)],NormalC PDF.Act_Draw_translate [(NotStrict,ConT Graphics.EasyRender.Internal.X),(NotStrict,ConT Graphics.EasyRender.Internal.Y)]] [])"
PDF.hs:22:3-27: Splicing declarations
    devArbitrary ''DrawAction
  ======>
    instance Arbitrary Basefont where
      arbitrary
        = sized go_aIDz
        where
            go_aIDz n_aIDA = oneof [return TimesRoman, return Helvetica]
    instance Arbitrary Color where
      arbitrary
        = sized go_aIDB
        where
            go_aIDB n_aIDC
              = oneof
                  [Color_RGB <$> resize (max 0 (n_aIDC - 1)) arbitrary
                   <*> resize (max 0 (n_aIDC - 1)) arbitrary
                   <*> resize (max 0 (n_aIDC - 1)) arbitrary,
                   Color_Gray <$> resize (max 0 (n_aIDC - 1)) arbitrary]
    instance Arbitrary Language where
      arbitrary
        = sized go_aIDD
        where
            go_aIDD n_aIDE
              = oneof
                  [return Language_PS, return Language_PDF, return Language_ASCII]
    instance Arbitrary CustomDef where
      arbitrary
        = sized go_aIDF
        where
            go_aIDF n_aIDG
              = CustomDef <$> resize (max 0 (n_aIDG - 1)) arbitrary
                <*> resize (max 0 (n_aIDG - 1)) arbitrary
    instance Arbitrary Font where
      arbitrary
        = sized go_aIDH
        where
            go_aIDH n_aIDI
              = Font <$> resize (max 0 (n_aIDI - 1)) arbitrary
                <*> resize (max 0 (n_aIDI - 1)) arbitrary
    instance Arbitrary DrawAction where
      arbitrary
        = sized go_aIDJ
        where
            go_aIDJ n_aIDK
              = if (n_aIDK <= 1) then
                    oneof
                      [Act_Draw_arc <$> resize (max 0 (n_aIDK - 1)) arbitrary
                       <*> resize (max 0 (n_aIDK - 1)) arbitrary
                       <*> resize (max 0 (n_aIDK - 1)) arbitrary
                       <*> resize (max 0 (n_aIDK - 1)) arbitrary
                       <*> resize (max 0 (n_aIDK - 1)) arbitrary,
                       Act_Draw_arc_append <$> resize (max 0 (n_aIDK - 1)) arbitrary
                       <*> resize (max 0 (n_aIDK - 1)) arbitrary
                       <*> resize (max 0 (n_aIDK - 1)) arbitrary
                       <*> resize (max 0 (n_aIDK - 1)) arbitrary
                       <*> resize (max 0 (n_aIDK - 1)) arbitrary,
                       return Act_Draw_clip, return Act_Draw_closepath,
                       Act_Draw_comment <$> resize (max 0 (n_aIDK - 1)) arbitrary,
                       Act_Draw_curveto <$> resize (max 0 (n_aIDK - 1)) arbitrary
                       <*> resize (max 0 (n_aIDK - 1)) arbitrary
                       <*> resize (max 0 (n_aIDK - 1)) arbitrary
                       <*> resize (max 0 (n_aIDK - 1)) arbitrary
                       <*> resize (max 0 (n_aIDK - 1)) arbitrary
                       <*> resize (max 0 (n_aIDK - 1)) arbitrary,
                       Act_Draw_endpage <$> resize (max 0 (n_aIDK - 1)) arbitrary
                       <*> resize (max 0 (n_aIDK - 1)) arbitrary,
                       Act_Draw_fill <$> resize (max 0 (n_aIDK - 1)) arbitrary,
                       Act_Draw_fillstroke <$> resize (max 0 (n_aIDK - 1)) arbitrary,
                       Act_Draw_lineto <$> resize (max 0 (n_aIDK - 1)) arbitrary
                       <*> resize (max 0 (n_aIDK - 1)) arbitrary,
                       Act_Draw_moveto <$> resize (max 0 (n_aIDK - 1)) arbitrary
                       <*> resize (max 0 (n_aIDK - 1)) arbitrary,
                       return Act_Draw_newpath,
                       Act_Draw_oval <$> resize (max 0 (n_aIDK - 1)) arbitrary
                       <*> resize (max 0 (n_aIDK - 1)) arbitrary
                       <*> resize (max 0 (n_aIDK - 1)) arbitrary
                       <*> resize (max 0 (n_aIDK - 1)) arbitrary,
                       Act_Draw_rectangle <$> resize (max 0 (n_aIDK - 1)) arbitrary
                       <*> resize (max 0 (n_aIDK - 1)) arbitrary
                       <*> resize (max 0 (n_aIDK - 1)) arbitrary
                       <*> resize (max 0 (n_aIDK - 1)) arbitrary,
                       Act_Draw_scale <$> resize (max 0 (n_aIDK - 1)) arbitrary
                       <*> resize (max 0 (n_aIDK - 1)) arbitrary,
                       Act_Draw_setcolor <$> resize (max 0 (n_aIDK - 1)) arbitrary,
                       Act_Draw_setlinewidth <$> resize (max 0 (n_aIDK - 1)) arbitrary,
                       return Act_Draw_stroke,
                       Act_Draw_textbox <$> resize (max 0 (n_aIDK - 1)) arbitrary
                       <*> resize (max 0 (n_aIDK - 1)) arbitrary
                       <*> resize (max 0 (n_aIDK - 1)) arbitrary
                       <*> resize (max 0 (n_aIDK - 1)) arbitrary
                       <*> resize (max 0 (n_aIDK - 1)) arbitrary
                       <*> resize (max 0 (n_aIDK - 1)) arbitrary
                       <*> resize (max 0 (n_aIDK - 1)) arbitrary
                       <*> resize (max 0 (n_aIDK - 1)) arbitrary
                       <*> resize (max 0 (n_aIDK - 1)) arbitrary,
                       Act_Draw_translate <$> resize (max 0 (n_aIDK - 1)) arbitrary
                       <*> resize (max 0 (n_aIDK - 1)) arbitrary]
                else
                    oneof
                      [Act_Draw_arc <$> resize (max 0 (n_aIDK - 1)) arbitrary
                       <*> resize (max 0 (n_aIDK - 1)) arbitrary
                       <*> resize (max 0 (n_aIDK - 1)) arbitrary
                       <*> resize (max 0 (n_aIDK - 1)) arbitrary
                       <*> resize (max 0 (n_aIDK - 1)) arbitrary,
                       Act_Draw_arc_append <$> resize (max 0 (n_aIDK - 1)) arbitrary
                       <*> resize (max 0 (n_aIDK - 1)) arbitrary
                       <*> resize (max 0 (n_aIDK - 1)) arbitrary
                       <*> resize (max 0 (n_aIDK - 1)) arbitrary
                       <*> resize (max 0 (n_aIDK - 1)) arbitrary,
                       return Act_Draw_clip, return Act_Draw_closepath,
                       Act_Draw_comment <$> resize (max 0 (n_aIDK - 1)) arbitrary,
                       Act_Draw_curveto <$> resize (max 0 (n_aIDK - 1)) arbitrary
                       <*> resize (max 0 (n_aIDK - 1)) arbitrary
                       <*> resize (max 0 (n_aIDK - 1)) arbitrary
                       <*> resize (max 0 (n_aIDK - 1)) arbitrary
                       <*> resize (max 0 (n_aIDK - 1)) arbitrary
                       <*> resize (max 0 (n_aIDK - 1)) arbitrary,
                       Act_Draw_draw_subroutine
                       <$> (listOf $ (resize (n_aIDK `div` 10) arbitrary))
                       <*> (listOf $ (resize (n_aIDK `div` 10) arbitrary)),
                       Act_Draw_endpage <$> resize (max 0 (n_aIDK - 1)) arbitrary
                       <*> resize (max 0 (n_aIDK - 1)) arbitrary,
                       Act_Draw_fill <$> resize (max 0 (n_aIDK - 1)) arbitrary,
                       Act_Draw_fillstroke <$> resize (max 0 (n_aIDK - 1)) arbitrary,
                       Act_Draw_lineto <$> resize (max 0 (n_aIDK - 1)) arbitrary
                       <*> resize (max 0 (n_aIDK - 1)) arbitrary,
                       Act_Draw_moveto <$> resize (max 0 (n_aIDK - 1)) arbitrary
                       <*> resize (max 0 (n_aIDK - 1)) arbitrary,
                       return Act_Draw_newpath,
                       Act_Draw_oval <$> resize (max 0 (n_aIDK - 1)) arbitrary
                       <*> resize (max 0 (n_aIDK - 1)) arbitrary
                       <*> resize (max 0 (n_aIDK - 1)) arbitrary
                       <*> resize (max 0 (n_aIDK - 1)) arbitrary,
                       Act_Draw_rectangle <$> resize (max 0 (n_aIDK - 1)) arbitrary
                       <*> resize (max 0 (n_aIDK - 1)) arbitrary
                       <*> resize (max 0 (n_aIDK - 1)) arbitrary
                       <*> resize (max 0 (n_aIDK - 1)) arbitrary,
                       Act_Draw_scale <$> resize (max 0 (n_aIDK - 1)) arbitrary
                       <*> resize (max 0 (n_aIDK - 1)) arbitrary,
                       Act_Draw_setcolor <$> resize (max 0 (n_aIDK - 1)) arbitrary,
                       Act_Draw_setlinewidth <$> resize (max 0 (n_aIDK - 1)) arbitrary,
                       return Act_Draw_stroke,
                       Act_Draw_textbox <$> resize (max 0 (n_aIDK - 1)) arbitrary
                       <*> resize (max 0 (n_aIDK - 1)) arbitrary
                       <*> resize (max 0 (n_aIDK - 1)) arbitrary
                       <*> resize (max 0 (n_aIDK - 1)) arbitrary
                       <*> resize (max 0 (n_aIDK - 1)) arbitrary
                       <*> resize (max 0 (n_aIDK - 1)) arbitrary
                       <*> resize (max 0 (n_aIDK - 1)) arbitrary
                       <*> resize (max 0 (n_aIDK - 1)) arbitrary
                       <*> resize (max 0 (n_aIDK - 1)) arbitrary,
                       Act_Draw_translate <$> resize (max 0 (n_aIDK - 1)) arbitrary
                       <*> resize (max 0 (n_aIDK - 1)) arbitrary]
"PreVisiting:PDF.DrawAction"
"Visiting: TyConI (DataD [] PDF.DrawAction [] [NormalC PDF.Act_Draw_arc [(NotStrict,ConT Graphics.EasyRender.Internal.X),(NotStrict,ConT Graphics.EasyRender.Internal.Y),(NotStrict,ConT GHC.Types.Double),(NotStrict,ConT GHC.Types.Double),(NotStrict,ConT GHC.Types.Double)],NormalC PDF.Act_Draw_arc_append [(NotStrict,ConT Graphics.EasyRender.Internal.X),(NotStrict,ConT Graphics.EasyRender.Internal.Y),(NotStrict,ConT GHC.Types.Double),(NotStrict,ConT GHC.Types.Double),(NotStrict,ConT GHC.Types.Double)],NormalC PDF.Act_Draw_clip [],NormalC PDF.Act_Draw_closepath [],NormalC PDF.Act_Draw_comment [(NotStrict,ConT GHC.Base.String)],NormalC PDF.Act_Draw_curveto [(NotStrict,ConT Graphics.EasyRender.Internal.X),(NotStrict,ConT Graphics.EasyRender.Internal.Y),(NotStrict,ConT Graphics.EasyRender.Internal.X),(NotStrict,ConT Graphics.EasyRender.Internal.Y),(NotStrict,ConT Graphics.EasyRender.Internal.X),(NotStrict,ConT Graphics.EasyRender.Internal.Y)],NormalC PDF.Act_Draw_draw_subroutine [(NotStrict,AppT ListT (ConT Graphics.EasyRender.Internal.CustomDef)),(NotStrict,AppT ListT (ConT PDF.DrawAction))],NormalC PDF.Act_Draw_endpage [(NotStrict,ConT Graphics.EasyRender.Internal.X),(NotStrict,ConT Graphics.EasyRender.Internal.Y)],NormalC PDF.Act_Draw_fill [(NotStrict,ConT Graphics.EasyRender.Internal.Color)],NormalC PDF.Act_Draw_fillstroke [(NotStrict,ConT Graphics.EasyRender.Internal.Color)],NormalC PDF.Act_Draw_lineto [(NotStrict,ConT Graphics.EasyRender.Internal.X),(NotStrict,ConT Graphics.EasyRender.Internal.Y)],NormalC PDF.Act_Draw_moveto [(NotStrict,ConT Graphics.EasyRender.Internal.X),(NotStrict,ConT Graphics.EasyRender.Internal.Y)],NormalC PDF.Act_Draw_newpath [],NormalC PDF.Act_Draw_oval [(NotStrict,ConT Graphics.EasyRender.Internal.X),(NotStrict,ConT Graphics.EasyRender.Internal.Y),(NotStrict,ConT Graphics.EasyRender.Internal.X),(NotStrict,ConT Graphics.EasyRender.Internal.Y)],NormalC PDF.Act_Draw_rectangle [(NotStrict,ConT Graphics.EasyRender.Internal.X),(NotStrict,ConT Graphics.EasyRender.Internal.Y),(NotStrict,ConT Graphics.EasyRender.Internal.X),(NotStrict,ConT Graphics.EasyRender.Internal.Y)],NormalC PDF.Act_Draw_scale [(NotStrict,ConT Graphics.EasyRender.Internal.X),(NotStrict,ConT Graphics.EasyRender.Internal.Y)],NormalC PDF.Act_Draw_setcolor [(NotStrict,ConT Graphics.EasyRender.Internal.Color)],NormalC PDF.Act_Draw_setlinewidth [(NotStrict,ConT GHC.Types.Double)],NormalC PDF.Act_Draw_stroke [],NormalC PDF.Act_Draw_textbox [(NotStrict,ConT Graphics.EasyRender.Internal.Alignment),(NotStrict,ConT Graphics.EasyRender.Internal.Font),(NotStrict,ConT Graphics.EasyRender.Internal.Color),(NotStrict,ConT Graphics.EasyRender.Internal.X),(NotStrict,ConT Graphics.EasyRender.Internal.Y),(NotStrict,ConT Graphics.EasyRender.Internal.X),(NotStrict,ConT Graphics.EasyRender.Internal.Y),(NotStrict,ConT GHC.Types.Double),(NotStrict,ConT GHC.Base.String)],NormalC PDF.Act_Draw_translate [(NotStrict,ConT Graphics.EasyRender.Internal.X),(NotStrict,ConT Graphics.EasyRender.Internal.Y)]] [])"
"PreVisiting:Graphics.EasyRender.Internal.X"
"Visiting: TyConI (TySynD Graphics.EasyRender.Internal.X [] (ConT GHC.Types.Double))"
"PreVisiting:Graphics.EasyRender.Internal.Y"
"Visiting: TyConI (TySynD Graphics.EasyRender.Internal.Y [] (ConT GHC.Types.Double))"
"PreVisiting:Graphics.EasyRender.Internal.CustomDef"
"Visiting: TyConI (DataD [] Graphics.EasyRender.Internal.CustomDef [] [NormalC Graphics.EasyRender.Internal.CustomDef [(NotStrict,ConT Graphics.EasyRender.Internal.Language),(NotStrict,ConT GHC.Base.String)]] [])"
"PreVisiting:Graphics.EasyRender.Internal.Language"
"Visiting: TyConI (DataD [] Graphics.EasyRender.Internal.Language [] [NormalC Graphics.EasyRender.Internal.Language_PS [],NormalC Graphics.EasyRender.Internal.Language_PDF [],NormalC Graphics.EasyRender.Internal.Language_ASCII []] [])"
"PreVisiting:Graphics.EasyRender.Internal.Color"
"Visiting: TyConI (DataD [] Graphics.EasyRender.Internal.Color [] [NormalC Graphics.EasyRender.Internal.Color_RGB [(NotStrict,ConT GHC.Types.Double),(NotStrict,ConT GHC.Types.Double),(NotStrict,ConT GHC.Types.Double)],NormalC Graphics.EasyRender.Internal.Color_Gray [(NotStrict,ConT GHC.Types.Double)]] [])"
"PreVisiting:Graphics.EasyRender.Internal.Alignment"
"Visiting: TyConI (TySynD Graphics.EasyRender.Internal.Alignment [] (ConT GHC.Types.Double))"
"PreVisiting:Graphics.EasyRender.Internal.Font"
"Visiting: TyConI (DataD [] Graphics.EasyRender.Internal.Font [] [NormalC Graphics.EasyRender.Internal.Font [(NotStrict,ConT Graphics.EasyRender.Internal.Basefont),(NotStrict,ConT GHC.Types.Double)]] [])"
"PreVisiting:Graphics.EasyRender.Internal.Basefont"
"Visiting: TyConI (DataD [] Graphics.EasyRender.Internal.Basefont [] [NormalC Graphics.EasyRender.Internal.TimesRoman [],NormalC Graphics.EasyRender.Internal.Helvetica []] [])"
PDF.hs:23:3-22: Splicing declarations
    devShow ''DrawAction
  ======>
    instance Show DrawAction where
      showsPrec p (Act_Draw_arc x1 x2 x3 x4 x5)
        = ((showParen (p > 10))
           $ ((showString "Act_Draw_arc ")
              . ((showsPrec 11 x1)
                 . ((showChar ' ')
                    . ((showsPrec 11 x2)
                       . ((showChar ' ')
                          . ((showsPrec 11 x3)
                             . ((showChar ' ')
                                . ((showsPrec 11 x4)
                                   . ((showChar ' ') . (showsPrec 11 x5)))))))))))
      showsPrec p (Act_Draw_arc_append x1 x2 x3 x4 x5)
        = ((showParen (p > 10))
           $ ((showString "Act_Draw_arc_append ")
              . ((showsPrec 11 x1)
                 . ((showChar ' ')
                    . ((showsPrec 11 x2)
                       . ((showChar ' ')
                          . ((showsPrec 11 x3)
                             . ((showChar ' ')
                                . ((showsPrec 11 x4)
                                   . ((showChar ' ') . (showsPrec 11 x5)))))))))))
      showsPrec _ Act_Draw_clip = showString "Act_Draw_clip"
      showsPrec _ Act_Draw_closepath = showString "Act_Draw_closepath"
      showsPrec p (Act_Draw_comment x1)
        = ((showParen (p > 10))
           $ ((showString "Act_Draw_comment ") . (showsPrec 11 x1)))
      showsPrec p (Act_Draw_curveto x1 x2 x3 x4 x5 x6)
        = ((showParen (p > 10))
           $ ((showString "Act_Draw_curveto ")
              . ((showsPrec 11 x1)
                 . ((showChar ' ')
                    . ((showsPrec 11 x2)
                       . ((showChar ' ')
                          . ((showsPrec 11 x3)
                             . ((showChar ' ')
                                . ((showsPrec 11 x4)
                                   . ((showChar ' ')
                                      . ((showsPrec 11 x5)
                                         . ((showChar ' ') . (showsPrec 11 x6)))))))))))))
      showsPrec p (Act_Draw_draw_subroutine x1 x2)
        = ((showParen (p > 10))
           $ ((showString "Act_Draw_draw_subroutine ")
              . ((showsPrec 11 x1) . ((showChar ' ') . (showsPrec 11 x2)))))
      showsPrec p (Act_Draw_endpage x1 x2)
        = ((showParen (p > 10))
           $ ((showString "Act_Draw_endpage ")
              . ((showsPrec 11 x1) . ((showChar ' ') . (showsPrec 11 x2)))))
      showsPrec p (Act_Draw_fill x1)
        = ((showParen (p > 10))
           $ ((showString "Act_Draw_fill ") . (showsPrec 11 x1)))
      showsPrec p (Act_Draw_fillstroke x1)
        = ((showParen (p > 10))
           $ ((showString "Act_Draw_fillstroke ") . (showsPrec 11 x1)))
      showsPrec p (Act_Draw_lineto x1 x2)
        = ((showParen (p > 10))
           $ ((showString "Act_Draw_lineto ")
              . ((showsPrec 11 x1) . ((showChar ' ') . (showsPrec 11 x2)))))
      showsPrec p (Act_Draw_moveto x1 x2)
        = ((showParen (p > 10))
           $ ((showString "Act_Draw_moveto ")
              . ((showsPrec 11 x1) . ((showChar ' ') . (showsPrec 11 x2)))))
      showsPrec _ Act_Draw_newpath = showString "Act_Draw_newpath"
      showsPrec p (Act_Draw_oval x1 x2 x3 x4)
        = ((showParen (p > 10))
           $ ((showString "Act_Draw_oval ")
              . ((showsPrec 11 x1)
                 . ((showChar ' ')
                    . ((showsPrec 11 x2)
                       . ((showChar ' ')
                          . ((showsPrec 11 x3) . ((showChar ' ') . (showsPrec 11 x4)))))))))
      showsPrec p (Act_Draw_rectangle x1 x2 x3 x4)
        = ((showParen (p > 10))
           $ ((showString "Act_Draw_rectangle ")
              . ((showsPrec 11 x1)
                 . ((showChar ' ')
                    . ((showsPrec 11 x2)
                       . ((showChar ' ')
                          . ((showsPrec 11 x3) . ((showChar ' ') . (showsPrec 11 x4)))))))))
      showsPrec p (Act_Draw_scale x1 x2)
        = ((showParen (p > 10))
           $ ((showString "Act_Draw_scale ")
              . ((showsPrec 11 x1) . ((showChar ' ') . (showsPrec 11 x2)))))
      showsPrec p (Act_Draw_setcolor x1)
        = ((showParen (p > 10))
           $ ((showString "Act_Draw_setcolor ") . (showsPrec 11 x1)))
      showsPrec p (Act_Draw_setlinewidth x1)
        = ((showParen (p > 10))
           $ ((showString "Act_Draw_setlinewidth ") . (showsPrec 11 x1)))
      showsPrec _ Act_Draw_stroke = showString "Act_Draw_stroke"
      showsPrec p (Act_Draw_textbox x1 x2 x3 x4 x5 x6 x7 x8 x9)
        = ((showParen (p > 10))
           $ ((showString "Act_Draw_textbox ")
              . ((showsPrec 11 x1)
                 . ((showChar ' ')
                    . ((showsPrec 11 x2)
                       . ((showChar ' ')
                          . ((showsPrec 11 x3)
                             . ((showChar ' ')
                                . ((showsPrec 11 x4)
                                   . ((showChar ' ')
                                      . ((showsPrec 11 x5)
                                         . ((showChar ' ')
                                            . ((showsPrec 11 x6)
                                               . ((showChar ' ')
                                                  . ((showsPrec 11 x7)
                                                     . ((showChar ' ')
                                                        . ((showsPrec 11 x8)
                                                           . ((showChar ' ')
                                                              . (showsPrec 11 x9)))))))))))))))))))
      showsPrec p (Act_Draw_translate x1 x2)
        = ((showParen (p > 10))
           $ ((showString "Act_Draw_translate ")
              . ((showsPrec 11 x1) . ((showChar ' ') . (showsPrec 11 x2)))))
Ok, modules loaded: PDF, DeriveMArbitrary, DeriveArbitrary, DeriveShow, ByteString, Strings, Vector, ProbGen.
Leaving GHCi.
