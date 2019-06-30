-- ------------------------------------------------------------

{- |
   Module     : Data.Char.Properties.UnicodeBlocks
   Copyright  : Copyright (C) 2010- Uwe Schmidt
   License    : MIT

   Maintainer : Uwe Schmidt (uwe@fh-wedel.de)
   Stability  : stable
   Portability: portable

   Unicode Code Blocks

   don't edit this module
   it's generated from 'http:\/\/www.unicode.org\/Public\/UNIDATA\/Blocks.txt'
-}

-- ------------------------------------------------------------

module Data.Char.Properties.UnicodeBlocks
  ( codeBlocks
  , elemCodeBlock
  , versionUnicode
  , isBasicLatin
  , isLatin1Supplement
  , isLatinExtendedA
  , isLatinExtendedB
  , isIPAExtensions
  , isSpacingModifierLetters
  , isCombiningDiacriticalMarks
  , isGreekandCoptic
  , isCyrillic
  , isCyrillicSupplement
  , isArmenian
  , isHebrew
  , isArabic
  , isSyriac
  , isArabicSupplement
  , isThaana
  , isNKo
  , isSamaritan
  , isMandaic
  , isSyriacSupplement
  , isArabicExtendedA
  , isDevanagari
  , isBengali
  , isGurmukhi
  , isGujarati
  , isOriya
  , isTamil
  , isTelugu
  , isKannada
  , isMalayalam
  , isSinhala
  , isThai
  , isLao
  , isTibetan
  , isMyanmar
  , isGeorgian
  , isHangulJamo
  , isEthiopic
  , isEthiopicSupplement
  , isCherokee
  , isUnifiedCanadianAboriginalSyllabics
  , isOgham
  , isRunic
  , isTagalog
  , isHanunoo
  , isBuhid
  , isTagbanwa
  , isKhmer
  , isMongolian
  , isUnifiedCanadianAboriginalSyllabicsExtended
  , isLimbu
  , isTaiLe
  , isNewTaiLue
  , isKhmerSymbols
  , isBuginese
  , isTaiTham
  , isCombiningDiacriticalMarksExtended
  , isBalinese
  , isSundanese
  , isBatak
  , isLepcha
  , isOlChiki
  , isCyrillicExtendedC
  , isGeorgianExtended
  , isSundaneseSupplement
  , isVedicExtensions
  , isPhoneticExtensions
  , isPhoneticExtensionsSupplement
  , isCombiningDiacriticalMarksSupplement
  , isLatinExtendedAdditional
  , isGreekExtended
  , isGeneralPunctuation
  , isSuperscriptsandSubscripts
  , isCurrencySymbols
  , isCombiningDiacriticalMarksforSymbols
  , isLetterlikeSymbols
  , isNumberForms
  , isArrows
  , isMathematicalOperators
  , isMiscellaneousTechnical
  , isControlPictures
  , isOpticalCharacterRecognition
  , isEnclosedAlphanumerics
  , isBoxDrawing
  , isBlockElements
  , isGeometricShapes
  , isMiscellaneousSymbols
  , isDingbats
  , isMiscellaneousMathematicalSymbolsA
  , isSupplementalArrowsA
  , isBraillePatterns
  , isSupplementalArrowsB
  , isMiscellaneousMathematicalSymbolsB
  , isSupplementalMathematicalOperators
  , isMiscellaneousSymbolsandArrows
  , isGlagolitic
  , isLatinExtendedC
  , isCoptic
  , isGeorgianSupplement
  , isTifinagh
  , isEthiopicExtended
  , isCyrillicExtendedA
  , isSupplementalPunctuation
  , isCJKRadicalsSupplement
  , isKangxiRadicals
  , isIdeographicDescriptionCharacters
  , isCJKSymbolsandPunctuation
  , isHiragana
  , isKatakana
  , isBopomofo
  , isHangulCompatibilityJamo
  , isKanbun
  , isBopomofoExtended
  , isCJKStrokes
  , isKatakanaPhoneticExtensions
  , isEnclosedCJKLettersandMonths
  , isCJKCompatibility
  , isCJKUnifiedIdeographsExtensionA
  , isYijingHexagramSymbols
  , isCJKUnifiedIdeographs
  , isYiSyllables
  , isYiRadicals
  , isLisu
  , isVai
  , isCyrillicExtendedB
  , isBamum
  , isModifierToneLetters
  , isLatinExtendedD
  , isSylotiNagri
  , isCommonIndicNumberForms
  , isPhagspa
  , isSaurashtra
  , isDevanagariExtended
  , isKayahLi
  , isRejang
  , isHangulJamoExtendedA
  , isJavanese
  , isMyanmarExtendedB
  , isCham
  , isMyanmarExtendedA
  , isTaiViet
  , isMeeteiMayekExtensions
  , isEthiopicExtendedA
  , isLatinExtendedE
  , isCherokeeSupplement
  , isMeeteiMayek
  , isHangulSyllables
  , isHangulJamoExtendedB
  , isHighSurrogates
  , isHighPrivateUseSurrogates
  , isLowSurrogates
  , isPrivateUseArea
  , isCJKCompatibilityIdeographs
  , isAlphabeticPresentationForms
  , isArabicPresentationFormsA
  , isVariationSelectors
  , isVerticalForms
  , isCombiningHalfMarks
  , isCJKCompatibilityForms
  , isSmallFormVariants
  , isArabicPresentationFormsB
  , isHalfwidthandFullwidthForms
  , isSpecials
  , isLinearBSyllabary
  , isLinearBIdeograms
  , isAegeanNumbers
  , isAncientGreekNumbers
  , isAncientSymbols
  , isPhaistosDisc
  , isLycian
  , isCarian
  , isCopticEpactNumbers
  , isOldItalic
  , isGothic
  , isOldPermic
  , isUgaritic
  , isOldPersian
  , isDeseret
  , isShavian
  , isOsmanya
  , isOsage
  , isElbasan
  , isCaucasianAlbanian
  , isLinearA
  , isCypriotSyllabary
  , isImperialAramaic
  , isPalmyrene
  , isNabataean
  , isHatran
  , isPhoenician
  , isLydian
  , isMeroiticHieroglyphs
  , isMeroiticCursive
  , isKharoshthi
  , isOldSouthArabian
  , isOldNorthArabian
  , isManichaean
  , isAvestan
  , isInscriptionalParthian
  , isInscriptionalPahlavi
  , isPsalterPahlavi
  , isOldTurkic
  , isOldHungarian
  , isHanifiRohingya
  , isRumiNumeralSymbols
  , isOldSogdian
  , isSogdian
  , isElymaic
  , isBrahmi
  , isKaithi
  , isSoraSompeng
  , isChakma
  , isMahajani
  , isSharada
  , isSinhalaArchaicNumbers
  , isKhojki
  , isMultani
  , isKhudawadi
  , isGrantha
  , isNewa
  , isTirhuta
  , isSiddham
  , isModi
  , isMongolianSupplement
  , isTakri
  , isAhom
  , isDogra
  , isWarangCiti
  , isNandinagari
  , isZanabazarSquare
  , isSoyombo
  , isPauCinHau
  , isBhaiksuki
  , isMarchen
  , isMasaramGondi
  , isGunjalaGondi
  , isMakasar
  , isTamilSupplement
  , isCuneiform
  , isCuneiformNumbersandPunctuation
  , isEarlyDynasticCuneiform
  , isEgyptianHieroglyphs
  , isEgyptianHieroglyphFormatControls
  , isAnatolianHieroglyphs
  , isBamumSupplement
  , isMro
  , isBassaVah
  , isPahawhHmong
  , isMedefaidrin
  , isMiao
  , isIdeographicSymbolsandPunctuation
  , isTangut
  , isTangutComponents
  , isKanaSupplement
  , isKanaExtendedA
  , isSmallKanaExtension
  , isNushu
  , isDuployan
  , isShorthandFormatControls
  , isByzantineMusicalSymbols
  , isMusicalSymbols
  , isAncientGreekMusicalNotation
  , isMayanNumerals
  , isTaiXuanJingSymbols
  , isCountingRodNumerals
  , isMathematicalAlphanumericSymbols
  , isSuttonSignWriting
  , isGlagoliticSupplement
  , isNyiakengPuachueHmong
  , isWancho
  , isMendeKikakui
  , isAdlam
  , isIndicSiyaqNumbers
  , isOttomanSiyaqNumbers
  , isArabicMathematicalAlphabeticSymbols
  , isMahjongTiles
  , isDominoTiles
  , isPlayingCards
  , isEnclosedAlphanumericSupplement
  , isEnclosedIdeographicSupplement
  , isMiscellaneousSymbolsandPictographs
  , isEmoticons
  , isOrnamentalDingbats
  , isTransportandMapSymbols
  , isAlchemicalSymbols
  , isGeometricShapesExtended
  , isSupplementalArrowsC
  , isSupplementalSymbolsandPictographs
  , isChessSymbols
  , isSymbolsandPictographsExtendedA
  , isCJKUnifiedIdeographsExtensionB
  , isCJKUnifiedIdeographsExtensionC
  , isCJKUnifiedIdeographsExtensionD
  , isCJKUnifiedIdeographsExtensionE
  , isCJKUnifiedIdeographsExtensionF
  , isCJKCompatibilityIdeographsSupplement
  , isTags
  , isVariationSelectorsSupplement
  , isSupplementaryPrivateUseAreaA
  , isSupplementaryPrivateUseAreaB
  )
where

-- ------------------------------------------------------------

versionUnicode :: String
versionUnicode = "12.1.0"

elemCodeBlock     :: Char -> String -> Bool
elemCodeBlock c b = maybe False (\ (lb, ub) -> c >= lb && c <= ub) $ lookup b codeBlocks

codeBlocks        :: [(String, (Char, Char))]
codeBlocks =
    [ ( "BasicLatin", ( '\x0000', '\x007F') )
    , ( "Latin-1Supplement", ( '\x0080', '\x00FF') )
    , ( "LatinExtended-A", ( '\x0100', '\x017F') )
    , ( "LatinExtended-B", ( '\x0180', '\x024F') )
    , ( "IPAExtensions", ( '\x0250', '\x02AF') )
    , ( "SpacingModifierLetters", ( '\x02B0', '\x02FF') )
    , ( "CombiningDiacriticalMarks", ( '\x0300', '\x036F') )
    , ( "GreekandCoptic", ( '\x0370', '\x03FF') )
    , ( "Cyrillic", ( '\x0400', '\x04FF') )
    , ( "CyrillicSupplement", ( '\x0500', '\x052F') )
    , ( "Armenian", ( '\x0530', '\x058F') )
    , ( "Hebrew", ( '\x0590', '\x05FF') )
    , ( "Arabic", ( '\x0600', '\x06FF') )
    , ( "Syriac", ( '\x0700', '\x074F') )
    , ( "ArabicSupplement", ( '\x0750', '\x077F') )
    , ( "Thaana", ( '\x0780', '\x07BF') )
    , ( "NKo", ( '\x07C0', '\x07FF') )
    , ( "Samaritan", ( '\x0800', '\x083F') )
    , ( "Mandaic", ( '\x0840', '\x085F') )
    , ( "SyriacSupplement", ( '\x0860', '\x086F') )
    , ( "ArabicExtended-A", ( '\x08A0', '\x08FF') )
    , ( "Devanagari", ( '\x0900', '\x097F') )
    , ( "Bengali", ( '\x0980', '\x09FF') )
    , ( "Gurmukhi", ( '\x0A00', '\x0A7F') )
    , ( "Gujarati", ( '\x0A80', '\x0AFF') )
    , ( "Oriya", ( '\x0B00', '\x0B7F') )
    , ( "Tamil", ( '\x0B80', '\x0BFF') )
    , ( "Telugu", ( '\x0C00', '\x0C7F') )
    , ( "Kannada", ( '\x0C80', '\x0CFF') )
    , ( "Malayalam", ( '\x0D00', '\x0D7F') )
    , ( "Sinhala", ( '\x0D80', '\x0DFF') )
    , ( "Thai", ( '\x0E00', '\x0E7F') )
    , ( "Lao", ( '\x0E80', '\x0EFF') )
    , ( "Tibetan", ( '\x0F00', '\x0FFF') )
    , ( "Myanmar", ( '\x1000', '\x109F') )
    , ( "Georgian", ( '\x10A0', '\x10FF') )
    , ( "HangulJamo", ( '\x1100', '\x11FF') )
    , ( "Ethiopic", ( '\x1200', '\x137F') )
    , ( "EthiopicSupplement", ( '\x1380', '\x139F') )
    , ( "Cherokee", ( '\x13A0', '\x13FF') )
    , ( "UnifiedCanadianAboriginalSyllabics", ( '\x1400', '\x167F') )
    , ( "Ogham", ( '\x1680', '\x169F') )
    , ( "Runic", ( '\x16A0', '\x16FF') )
    , ( "Tagalog", ( '\x1700', '\x171F') )
    , ( "Hanunoo", ( '\x1720', '\x173F') )
    , ( "Buhid", ( '\x1740', '\x175F') )
    , ( "Tagbanwa", ( '\x1760', '\x177F') )
    , ( "Khmer", ( '\x1780', '\x17FF') )
    , ( "Mongolian", ( '\x1800', '\x18AF') )
    , ( "UnifiedCanadianAboriginalSyllabicsExtended", ( '\x18B0', '\x18FF') )
    , ( "Limbu", ( '\x1900', '\x194F') )
    , ( "TaiLe", ( '\x1950', '\x197F') )
    , ( "NewTaiLue", ( '\x1980', '\x19DF') )
    , ( "KhmerSymbols", ( '\x19E0', '\x19FF') )
    , ( "Buginese", ( '\x1A00', '\x1A1F') )
    , ( "TaiTham", ( '\x1A20', '\x1AAF') )
    , ( "CombiningDiacriticalMarksExtended", ( '\x1AB0', '\x1AFF') )
    , ( "Balinese", ( '\x1B00', '\x1B7F') )
    , ( "Sundanese", ( '\x1B80', '\x1BBF') )
    , ( "Batak", ( '\x1BC0', '\x1BFF') )
    , ( "Lepcha", ( '\x1C00', '\x1C4F') )
    , ( "OlChiki", ( '\x1C50', '\x1C7F') )
    , ( "CyrillicExtended-C", ( '\x1C80', '\x1C8F') )
    , ( "GeorgianExtended", ( '\x1C90', '\x1CBF') )
    , ( "SundaneseSupplement", ( '\x1CC0', '\x1CCF') )
    , ( "VedicExtensions", ( '\x1CD0', '\x1CFF') )
    , ( "PhoneticExtensions", ( '\x1D00', '\x1D7F') )
    , ( "PhoneticExtensionsSupplement", ( '\x1D80', '\x1DBF') )
    , ( "CombiningDiacriticalMarksSupplement", ( '\x1DC0', '\x1DFF') )
    , ( "LatinExtendedAdditional", ( '\x1E00', '\x1EFF') )
    , ( "GreekExtended", ( '\x1F00', '\x1FFF') )
    , ( "GeneralPunctuation", ( '\x2000', '\x206F') )
    , ( "SuperscriptsandSubscripts", ( '\x2070', '\x209F') )
    , ( "CurrencySymbols", ( '\x20A0', '\x20CF') )
    , ( "CombiningDiacriticalMarksforSymbols", ( '\x20D0', '\x20FF') )
    , ( "LetterlikeSymbols", ( '\x2100', '\x214F') )
    , ( "NumberForms", ( '\x2150', '\x218F') )
    , ( "Arrows", ( '\x2190', '\x21FF') )
    , ( "MathematicalOperators", ( '\x2200', '\x22FF') )
    , ( "MiscellaneousTechnical", ( '\x2300', '\x23FF') )
    , ( "ControlPictures", ( '\x2400', '\x243F') )
    , ( "OpticalCharacterRecognition", ( '\x2440', '\x245F') )
    , ( "EnclosedAlphanumerics", ( '\x2460', '\x24FF') )
    , ( "BoxDrawing", ( '\x2500', '\x257F') )
    , ( "BlockElements", ( '\x2580', '\x259F') )
    , ( "GeometricShapes", ( '\x25A0', '\x25FF') )
    , ( "MiscellaneousSymbols", ( '\x2600', '\x26FF') )
    , ( "Dingbats", ( '\x2700', '\x27BF') )
    , ( "MiscellaneousMathematicalSymbols-A", ( '\x27C0', '\x27EF') )
    , ( "SupplementalArrows-A", ( '\x27F0', '\x27FF') )
    , ( "BraillePatterns", ( '\x2800', '\x28FF') )
    , ( "SupplementalArrows-B", ( '\x2900', '\x297F') )
    , ( "MiscellaneousMathematicalSymbols-B", ( '\x2980', '\x29FF') )
    , ( "SupplementalMathematicalOperators", ( '\x2A00', '\x2AFF') )
    , ( "MiscellaneousSymbolsandArrows", ( '\x2B00', '\x2BFF') )
    , ( "Glagolitic", ( '\x2C00', '\x2C5F') )
    , ( "LatinExtended-C", ( '\x2C60', '\x2C7F') )
    , ( "Coptic", ( '\x2C80', '\x2CFF') )
    , ( "GeorgianSupplement", ( '\x2D00', '\x2D2F') )
    , ( "Tifinagh", ( '\x2D30', '\x2D7F') )
    , ( "EthiopicExtended", ( '\x2D80', '\x2DDF') )
    , ( "CyrillicExtended-A", ( '\x2DE0', '\x2DFF') )
    , ( "SupplementalPunctuation", ( '\x2E00', '\x2E7F') )
    , ( "CJKRadicalsSupplement", ( '\x2E80', '\x2EFF') )
    , ( "KangxiRadicals", ( '\x2F00', '\x2FDF') )
    , ( "IdeographicDescriptionCharacters", ( '\x2FF0', '\x2FFF') )
    , ( "CJKSymbolsandPunctuation", ( '\x3000', '\x303F') )
    , ( "Hiragana", ( '\x3040', '\x309F') )
    , ( "Katakana", ( '\x30A0', '\x30FF') )
    , ( "Bopomofo", ( '\x3100', '\x312F') )
    , ( "HangulCompatibilityJamo", ( '\x3130', '\x318F') )
    , ( "Kanbun", ( '\x3190', '\x319F') )
    , ( "BopomofoExtended", ( '\x31A0', '\x31BF') )
    , ( "CJKStrokes", ( '\x31C0', '\x31EF') )
    , ( "KatakanaPhoneticExtensions", ( '\x31F0', '\x31FF') )
    , ( "EnclosedCJKLettersandMonths", ( '\x3200', '\x32FF') )
    , ( "CJKCompatibility", ( '\x3300', '\x33FF') )
    , ( "CJKUnifiedIdeographsExtensionA", ( '\x3400', '\x4DBF') )
    , ( "YijingHexagramSymbols", ( '\x4DC0', '\x4DFF') )
    , ( "CJKUnifiedIdeographs", ( '\x4E00', '\x9FFF') )
    , ( "YiSyllables", ( '\xA000', '\xA48F') )
    , ( "YiRadicals", ( '\xA490', '\xA4CF') )
    , ( "Lisu", ( '\xA4D0', '\xA4FF') )
    , ( "Vai", ( '\xA500', '\xA63F') )
    , ( "CyrillicExtended-B", ( '\xA640', '\xA69F') )
    , ( "Bamum", ( '\xA6A0', '\xA6FF') )
    , ( "ModifierToneLetters", ( '\xA700', '\xA71F') )
    , ( "LatinExtended-D", ( '\xA720', '\xA7FF') )
    , ( "SylotiNagri", ( '\xA800', '\xA82F') )
    , ( "CommonIndicNumberForms", ( '\xA830', '\xA83F') )
    , ( "Phags-pa", ( '\xA840', '\xA87F') )
    , ( "Saurashtra", ( '\xA880', '\xA8DF') )
    , ( "DevanagariExtended", ( '\xA8E0', '\xA8FF') )
    , ( "KayahLi", ( '\xA900', '\xA92F') )
    , ( "Rejang", ( '\xA930', '\xA95F') )
    , ( "HangulJamoExtended-A", ( '\xA960', '\xA97F') )
    , ( "Javanese", ( '\xA980', '\xA9DF') )
    , ( "MyanmarExtended-B", ( '\xA9E0', '\xA9FF') )
    , ( "Cham", ( '\xAA00', '\xAA5F') )
    , ( "MyanmarExtended-A", ( '\xAA60', '\xAA7F') )
    , ( "TaiViet", ( '\xAA80', '\xAADF') )
    , ( "MeeteiMayekExtensions", ( '\xAAE0', '\xAAFF') )
    , ( "EthiopicExtended-A", ( '\xAB00', '\xAB2F') )
    , ( "LatinExtended-E", ( '\xAB30', '\xAB6F') )
    , ( "CherokeeSupplement", ( '\xAB70', '\xABBF') )
    , ( "MeeteiMayek", ( '\xABC0', '\xABFF') )
    , ( "HangulSyllables", ( '\xAC00', '\xD7AF') )
    , ( "HangulJamoExtended-B", ( '\xD7B0', '\xD7FF') )
    , ( "HighSurrogates", ( '\xD800', '\xDB7F') )
    , ( "HighPrivateUseSurrogates", ( '\xDB80', '\xDBFF') )
    , ( "LowSurrogates", ( '\xDC00', '\xDFFF') )
    , ( "PrivateUseArea", ( '\xE000', '\xF8FF') )
    , ( "CJKCompatibilityIdeographs", ( '\xF900', '\xFAFF') )
    , ( "AlphabeticPresentationForms", ( '\xFB00', '\xFB4F') )
    , ( "ArabicPresentationForms-A", ( '\xFB50', '\xFDFF') )
    , ( "VariationSelectors", ( '\xFE00', '\xFE0F') )
    , ( "VerticalForms", ( '\xFE10', '\xFE1F') )
    , ( "CombiningHalfMarks", ( '\xFE20', '\xFE2F') )
    , ( "CJKCompatibilityForms", ( '\xFE30', '\xFE4F') )
    , ( "SmallFormVariants", ( '\xFE50', '\xFE6F') )
    , ( "ArabicPresentationForms-B", ( '\xFE70', '\xFEFF') )
    , ( "HalfwidthandFullwidthForms", ( '\xFF00', '\xFFEF') )
    , ( "Specials", ( '\xFFF0', '\xFFFF') )
    , ( "LinearBSyllabary", ( '\x10000', '\x1007F') )
    , ( "LinearBIdeograms", ( '\x10080', '\x100FF') )
    , ( "AegeanNumbers", ( '\x10100', '\x1013F') )
    , ( "AncientGreekNumbers", ( '\x10140', '\x1018F') )
    , ( "AncientSymbols", ( '\x10190', '\x101CF') )
    , ( "PhaistosDisc", ( '\x101D0', '\x101FF') )
    , ( "Lycian", ( '\x10280', '\x1029F') )
    , ( "Carian", ( '\x102A0', '\x102DF') )
    , ( "CopticEpactNumbers", ( '\x102E0', '\x102FF') )
    , ( "OldItalic", ( '\x10300', '\x1032F') )
    , ( "Gothic", ( '\x10330', '\x1034F') )
    , ( "OldPermic", ( '\x10350', '\x1037F') )
    , ( "Ugaritic", ( '\x10380', '\x1039F') )
    , ( "OldPersian", ( '\x103A0', '\x103DF') )
    , ( "Deseret", ( '\x10400', '\x1044F') )
    , ( "Shavian", ( '\x10450', '\x1047F') )
    , ( "Osmanya", ( '\x10480', '\x104AF') )
    , ( "Osage", ( '\x104B0', '\x104FF') )
    , ( "Elbasan", ( '\x10500', '\x1052F') )
    , ( "CaucasianAlbanian", ( '\x10530', '\x1056F') )
    , ( "LinearA", ( '\x10600', '\x1077F') )
    , ( "CypriotSyllabary", ( '\x10800', '\x1083F') )
    , ( "ImperialAramaic", ( '\x10840', '\x1085F') )
    , ( "Palmyrene", ( '\x10860', '\x1087F') )
    , ( "Nabataean", ( '\x10880', '\x108AF') )
    , ( "Hatran", ( '\x108E0', '\x108FF') )
    , ( "Phoenician", ( '\x10900', '\x1091F') )
    , ( "Lydian", ( '\x10920', '\x1093F') )
    , ( "MeroiticHieroglyphs", ( '\x10980', '\x1099F') )
    , ( "MeroiticCursive", ( '\x109A0', '\x109FF') )
    , ( "Kharoshthi", ( '\x10A00', '\x10A5F') )
    , ( "OldSouthArabian", ( '\x10A60', '\x10A7F') )
    , ( "OldNorthArabian", ( '\x10A80', '\x10A9F') )
    , ( "Manichaean", ( '\x10AC0', '\x10AFF') )
    , ( "Avestan", ( '\x10B00', '\x10B3F') )
    , ( "InscriptionalParthian", ( '\x10B40', '\x10B5F') )
    , ( "InscriptionalPahlavi", ( '\x10B60', '\x10B7F') )
    , ( "PsalterPahlavi", ( '\x10B80', '\x10BAF') )
    , ( "OldTurkic", ( '\x10C00', '\x10C4F') )
    , ( "OldHungarian", ( '\x10C80', '\x10CFF') )
    , ( "HanifiRohingya", ( '\x10D00', '\x10D3F') )
    , ( "RumiNumeralSymbols", ( '\x10E60', '\x10E7F') )
    , ( "OldSogdian", ( '\x10F00', '\x10F2F') )
    , ( "Sogdian", ( '\x10F30', '\x10F6F') )
    , ( "Elymaic", ( '\x10FE0', '\x10FFF') )
    , ( "Brahmi", ( '\x11000', '\x1107F') )
    , ( "Kaithi", ( '\x11080', '\x110CF') )
    , ( "SoraSompeng", ( '\x110D0', '\x110FF') )
    , ( "Chakma", ( '\x11100', '\x1114F') )
    , ( "Mahajani", ( '\x11150', '\x1117F') )
    , ( "Sharada", ( '\x11180', '\x111DF') )
    , ( "SinhalaArchaicNumbers", ( '\x111E0', '\x111FF') )
    , ( "Khojki", ( '\x11200', '\x1124F') )
    , ( "Multani", ( '\x11280', '\x112AF') )
    , ( "Khudawadi", ( '\x112B0', '\x112FF') )
    , ( "Grantha", ( '\x11300', '\x1137F') )
    , ( "Newa", ( '\x11400', '\x1147F') )
    , ( "Tirhuta", ( '\x11480', '\x114DF') )
    , ( "Siddham", ( '\x11580', '\x115FF') )
    , ( "Modi", ( '\x11600', '\x1165F') )
    , ( "MongolianSupplement", ( '\x11660', '\x1167F') )
    , ( "Takri", ( '\x11680', '\x116CF') )
    , ( "Ahom", ( '\x11700', '\x1173F') )
    , ( "Dogra", ( '\x11800', '\x1184F') )
    , ( "WarangCiti", ( '\x118A0', '\x118FF') )
    , ( "Nandinagari", ( '\x119A0', '\x119FF') )
    , ( "ZanabazarSquare", ( '\x11A00', '\x11A4F') )
    , ( "Soyombo", ( '\x11A50', '\x11AAF') )
    , ( "PauCinHau", ( '\x11AC0', '\x11AFF') )
    , ( "Bhaiksuki", ( '\x11C00', '\x11C6F') )
    , ( "Marchen", ( '\x11C70', '\x11CBF') )
    , ( "MasaramGondi", ( '\x11D00', '\x11D5F') )
    , ( "GunjalaGondi", ( '\x11D60', '\x11DAF') )
    , ( "Makasar", ( '\x11EE0', '\x11EFF') )
    , ( "TamilSupplement", ( '\x11FC0', '\x11FFF') )
    , ( "Cuneiform", ( '\x12000', '\x123FF') )
    , ( "CuneiformNumbersandPunctuation", ( '\x12400', '\x1247F') )
    , ( "EarlyDynasticCuneiform", ( '\x12480', '\x1254F') )
    , ( "EgyptianHieroglyphs", ( '\x13000', '\x1342F') )
    , ( "EgyptianHieroglyphFormatControls", ( '\x13430', '\x1343F') )
    , ( "AnatolianHieroglyphs", ( '\x14400', '\x1467F') )
    , ( "BamumSupplement", ( '\x16800', '\x16A3F') )
    , ( "Mro", ( '\x16A40', '\x16A6F') )
    , ( "BassaVah", ( '\x16AD0', '\x16AFF') )
    , ( "PahawhHmong", ( '\x16B00', '\x16B8F') )
    , ( "Medefaidrin", ( '\x16E40', '\x16E9F') )
    , ( "Miao", ( '\x16F00', '\x16F9F') )
    , ( "IdeographicSymbolsandPunctuation", ( '\x16FE0', '\x16FFF') )
    , ( "Tangut", ( '\x17000', '\x187FF') )
    , ( "TangutComponents", ( '\x18800', '\x18AFF') )
    , ( "KanaSupplement", ( '\x1B000', '\x1B0FF') )
    , ( "KanaExtended-A", ( '\x1B100', '\x1B12F') )
    , ( "SmallKanaExtension", ( '\x1B130', '\x1B16F') )
    , ( "Nushu", ( '\x1B170', '\x1B2FF') )
    , ( "Duployan", ( '\x1BC00', '\x1BC9F') )
    , ( "ShorthandFormatControls", ( '\x1BCA0', '\x1BCAF') )
    , ( "ByzantineMusicalSymbols", ( '\x1D000', '\x1D0FF') )
    , ( "MusicalSymbols", ( '\x1D100', '\x1D1FF') )
    , ( "AncientGreekMusicalNotation", ( '\x1D200', '\x1D24F') )
    , ( "MayanNumerals", ( '\x1D2E0', '\x1D2FF') )
    , ( "TaiXuanJingSymbols", ( '\x1D300', '\x1D35F') )
    , ( "CountingRodNumerals", ( '\x1D360', '\x1D37F') )
    , ( "MathematicalAlphanumericSymbols", ( '\x1D400', '\x1D7FF') )
    , ( "SuttonSignWriting", ( '\x1D800', '\x1DAAF') )
    , ( "GlagoliticSupplement", ( '\x1E000', '\x1E02F') )
    , ( "NyiakengPuachueHmong", ( '\x1E100', '\x1E14F') )
    , ( "Wancho", ( '\x1E2C0', '\x1E2FF') )
    , ( "MendeKikakui", ( '\x1E800', '\x1E8DF') )
    , ( "Adlam", ( '\x1E900', '\x1E95F') )
    , ( "IndicSiyaqNumbers", ( '\x1EC70', '\x1ECBF') )
    , ( "OttomanSiyaqNumbers", ( '\x1ED00', '\x1ED4F') )
    , ( "ArabicMathematicalAlphabeticSymbols", ( '\x1EE00', '\x1EEFF') )
    , ( "MahjongTiles", ( '\x1F000', '\x1F02F') )
    , ( "DominoTiles", ( '\x1F030', '\x1F09F') )
    , ( "PlayingCards", ( '\x1F0A0', '\x1F0FF') )
    , ( "EnclosedAlphanumericSupplement", ( '\x1F100', '\x1F1FF') )
    , ( "EnclosedIdeographicSupplement", ( '\x1F200', '\x1F2FF') )
    , ( "MiscellaneousSymbolsandPictographs", ( '\x1F300', '\x1F5FF') )
    , ( "Emoticons", ( '\x1F600', '\x1F64F') )
    , ( "OrnamentalDingbats", ( '\x1F650', '\x1F67F') )
    , ( "TransportandMapSymbols", ( '\x1F680', '\x1F6FF') )
    , ( "AlchemicalSymbols", ( '\x1F700', '\x1F77F') )
    , ( "GeometricShapesExtended", ( '\x1F780', '\x1F7FF') )
    , ( "SupplementalArrows-C", ( '\x1F800', '\x1F8FF') )
    , ( "SupplementalSymbolsandPictographs", ( '\x1F900', '\x1F9FF') )
    , ( "ChessSymbols", ( '\x1FA00', '\x1FA6F') )
    , ( "SymbolsandPictographsExtended-A", ( '\x1FA70', '\x1FAFF') )
    , ( "CJKUnifiedIdeographsExtensionB", ( '\x20000', '\x2A6DF') )
    , ( "CJKUnifiedIdeographsExtensionC", ( '\x2A700', '\x2B73F') )
    , ( "CJKUnifiedIdeographsExtensionD", ( '\x2B740', '\x2B81F') )
    , ( "CJKUnifiedIdeographsExtensionE", ( '\x2B820', '\x2CEAF') )
    , ( "CJKUnifiedIdeographsExtensionF", ( '\x2CEB0', '\x2EBEF') )
    , ( "CJKCompatibilityIdeographsSupplement", ( '\x2F800', '\x2FA1F') )
    , ( "Tags", ( '\xE0000', '\xE007F') )
    , ( "VariationSelectorsSupplement", ( '\xE0100', '\xE01EF') )
    , ( "SupplementaryPrivateUseArea-A", ( '\xF0000', '\xFFFFF') )
    , ( "SupplementaryPrivateUseArea-B", ( '\x100000', '\x10FFFF') )
    ]

-- ------------------------------------------------------------

isBasicLatin   :: Char -> Bool
isBasicLatin c = c >= '\x0000' && c <= '\x007F'

isLatin1Supplement   :: Char -> Bool
isLatin1Supplement c = c >= '\x0080' && c <= '\x00FF'

isLatinExtendedA   :: Char -> Bool
isLatinExtendedA c = c >= '\x0100' && c <= '\x017F'

isLatinExtendedB   :: Char -> Bool
isLatinExtendedB c = c >= '\x0180' && c <= '\x024F'

isIPAExtensions   :: Char -> Bool
isIPAExtensions c = c >= '\x0250' && c <= '\x02AF'

isSpacingModifierLetters   :: Char -> Bool
isSpacingModifierLetters c = c >= '\x02B0' && c <= '\x02FF'

isCombiningDiacriticalMarks   :: Char -> Bool
isCombiningDiacriticalMarks c = c >= '\x0300' && c <= '\x036F'

isGreekandCoptic   :: Char -> Bool
isGreekandCoptic c = c >= '\x0370' && c <= '\x03FF'

isCyrillic   :: Char -> Bool
isCyrillic c = c >= '\x0400' && c <= '\x04FF'

isCyrillicSupplement   :: Char -> Bool
isCyrillicSupplement c = c >= '\x0500' && c <= '\x052F'

isArmenian   :: Char -> Bool
isArmenian c = c >= '\x0530' && c <= '\x058F'

isHebrew   :: Char -> Bool
isHebrew c = c >= '\x0590' && c <= '\x05FF'

isArabic   :: Char -> Bool
isArabic c = c >= '\x0600' && c <= '\x06FF'

isSyriac   :: Char -> Bool
isSyriac c = c >= '\x0700' && c <= '\x074F'

isArabicSupplement   :: Char -> Bool
isArabicSupplement c = c >= '\x0750' && c <= '\x077F'

isThaana   :: Char -> Bool
isThaana c = c >= '\x0780' && c <= '\x07BF'

isNKo   :: Char -> Bool
isNKo c = c >= '\x07C0' && c <= '\x07FF'

isSamaritan   :: Char -> Bool
isSamaritan c = c >= '\x0800' && c <= '\x083F'

isMandaic   :: Char -> Bool
isMandaic c = c >= '\x0840' && c <= '\x085F'

isSyriacSupplement   :: Char -> Bool
isSyriacSupplement c = c >= '\x0860' && c <= '\x086F'

isArabicExtendedA   :: Char -> Bool
isArabicExtendedA c = c >= '\x08A0' && c <= '\x08FF'

isDevanagari   :: Char -> Bool
isDevanagari c = c >= '\x0900' && c <= '\x097F'

isBengali   :: Char -> Bool
isBengali c = c >= '\x0980' && c <= '\x09FF'

isGurmukhi   :: Char -> Bool
isGurmukhi c = c >= '\x0A00' && c <= '\x0A7F'

isGujarati   :: Char -> Bool
isGujarati c = c >= '\x0A80' && c <= '\x0AFF'

isOriya   :: Char -> Bool
isOriya c = c >= '\x0B00' && c <= '\x0B7F'

isTamil   :: Char -> Bool
isTamil c = c >= '\x0B80' && c <= '\x0BFF'

isTelugu   :: Char -> Bool
isTelugu c = c >= '\x0C00' && c <= '\x0C7F'

isKannada   :: Char -> Bool
isKannada c = c >= '\x0C80' && c <= '\x0CFF'

isMalayalam   :: Char -> Bool
isMalayalam c = c >= '\x0D00' && c <= '\x0D7F'

isSinhala   :: Char -> Bool
isSinhala c = c >= '\x0D80' && c <= '\x0DFF'

isThai   :: Char -> Bool
isThai c = c >= '\x0E00' && c <= '\x0E7F'

isLao   :: Char -> Bool
isLao c = c >= '\x0E80' && c <= '\x0EFF'

isTibetan   :: Char -> Bool
isTibetan c = c >= '\x0F00' && c <= '\x0FFF'

isMyanmar   :: Char -> Bool
isMyanmar c = c >= '\x1000' && c <= '\x109F'

isGeorgian   :: Char -> Bool
isGeorgian c = c >= '\x10A0' && c <= '\x10FF'

isHangulJamo   :: Char -> Bool
isHangulJamo c = c >= '\x1100' && c <= '\x11FF'

isEthiopic   :: Char -> Bool
isEthiopic c = c >= '\x1200' && c <= '\x137F'

isEthiopicSupplement   :: Char -> Bool
isEthiopicSupplement c = c >= '\x1380' && c <= '\x139F'

isCherokee   :: Char -> Bool
isCherokee c = c >= '\x13A0' && c <= '\x13FF'

isUnifiedCanadianAboriginalSyllabics   :: Char -> Bool
isUnifiedCanadianAboriginalSyllabics c = c >= '\x1400' && c <= '\x167F'

isOgham   :: Char -> Bool
isOgham c = c >= '\x1680' && c <= '\x169F'

isRunic   :: Char -> Bool
isRunic c = c >= '\x16A0' && c <= '\x16FF'

isTagalog   :: Char -> Bool
isTagalog c = c >= '\x1700' && c <= '\x171F'

isHanunoo   :: Char -> Bool
isHanunoo c = c >= '\x1720' && c <= '\x173F'

isBuhid   :: Char -> Bool
isBuhid c = c >= '\x1740' && c <= '\x175F'

isTagbanwa   :: Char -> Bool
isTagbanwa c = c >= '\x1760' && c <= '\x177F'

isKhmer   :: Char -> Bool
isKhmer c = c >= '\x1780' && c <= '\x17FF'

isMongolian   :: Char -> Bool
isMongolian c = c >= '\x1800' && c <= '\x18AF'

isUnifiedCanadianAboriginalSyllabicsExtended   :: Char -> Bool
isUnifiedCanadianAboriginalSyllabicsExtended c = c >= '\x18B0' && c <= '\x18FF'

isLimbu   :: Char -> Bool
isLimbu c = c >= '\x1900' && c <= '\x194F'

isTaiLe   :: Char -> Bool
isTaiLe c = c >= '\x1950' && c <= '\x197F'

isNewTaiLue   :: Char -> Bool
isNewTaiLue c = c >= '\x1980' && c <= '\x19DF'

isKhmerSymbols   :: Char -> Bool
isKhmerSymbols c = c >= '\x19E0' && c <= '\x19FF'

isBuginese   :: Char -> Bool
isBuginese c = c >= '\x1A00' && c <= '\x1A1F'

isTaiTham   :: Char -> Bool
isTaiTham c = c >= '\x1A20' && c <= '\x1AAF'

isCombiningDiacriticalMarksExtended   :: Char -> Bool
isCombiningDiacriticalMarksExtended c = c >= '\x1AB0' && c <= '\x1AFF'

isBalinese   :: Char -> Bool
isBalinese c = c >= '\x1B00' && c <= '\x1B7F'

isSundanese   :: Char -> Bool
isSundanese c = c >= '\x1B80' && c <= '\x1BBF'

isBatak   :: Char -> Bool
isBatak c = c >= '\x1BC0' && c <= '\x1BFF'

isLepcha   :: Char -> Bool
isLepcha c = c >= '\x1C00' && c <= '\x1C4F'

isOlChiki   :: Char -> Bool
isOlChiki c = c >= '\x1C50' && c <= '\x1C7F'

isCyrillicExtendedC   :: Char -> Bool
isCyrillicExtendedC c = c >= '\x1C80' && c <= '\x1C8F'

isGeorgianExtended   :: Char -> Bool
isGeorgianExtended c = c >= '\x1C90' && c <= '\x1CBF'

isSundaneseSupplement   :: Char -> Bool
isSundaneseSupplement c = c >= '\x1CC0' && c <= '\x1CCF'

isVedicExtensions   :: Char -> Bool
isVedicExtensions c = c >= '\x1CD0' && c <= '\x1CFF'

isPhoneticExtensions   :: Char -> Bool
isPhoneticExtensions c = c >= '\x1D00' && c <= '\x1D7F'

isPhoneticExtensionsSupplement   :: Char -> Bool
isPhoneticExtensionsSupplement c = c >= '\x1D80' && c <= '\x1DBF'

isCombiningDiacriticalMarksSupplement   :: Char -> Bool
isCombiningDiacriticalMarksSupplement c = c >= '\x1DC0' && c <= '\x1DFF'

isLatinExtendedAdditional   :: Char -> Bool
isLatinExtendedAdditional c = c >= '\x1E00' && c <= '\x1EFF'

isGreekExtended   :: Char -> Bool
isGreekExtended c = c >= '\x1F00' && c <= '\x1FFF'

isGeneralPunctuation   :: Char -> Bool
isGeneralPunctuation c = c >= '\x2000' && c <= '\x206F'

isSuperscriptsandSubscripts   :: Char -> Bool
isSuperscriptsandSubscripts c = c >= '\x2070' && c <= '\x209F'

isCurrencySymbols   :: Char -> Bool
isCurrencySymbols c = c >= '\x20A0' && c <= '\x20CF'

isCombiningDiacriticalMarksforSymbols   :: Char -> Bool
isCombiningDiacriticalMarksforSymbols c = c >= '\x20D0' && c <= '\x20FF'

isLetterlikeSymbols   :: Char -> Bool
isLetterlikeSymbols c = c >= '\x2100' && c <= '\x214F'

isNumberForms   :: Char -> Bool
isNumberForms c = c >= '\x2150' && c <= '\x218F'

isArrows   :: Char -> Bool
isArrows c = c >= '\x2190' && c <= '\x21FF'

isMathematicalOperators   :: Char -> Bool
isMathematicalOperators c = c >= '\x2200' && c <= '\x22FF'

isMiscellaneousTechnical   :: Char -> Bool
isMiscellaneousTechnical c = c >= '\x2300' && c <= '\x23FF'

isControlPictures   :: Char -> Bool
isControlPictures c = c >= '\x2400' && c <= '\x243F'

isOpticalCharacterRecognition   :: Char -> Bool
isOpticalCharacterRecognition c = c >= '\x2440' && c <= '\x245F'

isEnclosedAlphanumerics   :: Char -> Bool
isEnclosedAlphanumerics c = c >= '\x2460' && c <= '\x24FF'

isBoxDrawing   :: Char -> Bool
isBoxDrawing c = c >= '\x2500' && c <= '\x257F'

isBlockElements   :: Char -> Bool
isBlockElements c = c >= '\x2580' && c <= '\x259F'

isGeometricShapes   :: Char -> Bool
isGeometricShapes c = c >= '\x25A0' && c <= '\x25FF'

isMiscellaneousSymbols   :: Char -> Bool
isMiscellaneousSymbols c = c >= '\x2600' && c <= '\x26FF'

isDingbats   :: Char -> Bool
isDingbats c = c >= '\x2700' && c <= '\x27BF'

isMiscellaneousMathematicalSymbolsA   :: Char -> Bool
isMiscellaneousMathematicalSymbolsA c = c >= '\x27C0' && c <= '\x27EF'

isSupplementalArrowsA   :: Char -> Bool
isSupplementalArrowsA c = c >= '\x27F0' && c <= '\x27FF'

isBraillePatterns   :: Char -> Bool
isBraillePatterns c = c >= '\x2800' && c <= '\x28FF'

isSupplementalArrowsB   :: Char -> Bool
isSupplementalArrowsB c = c >= '\x2900' && c <= '\x297F'

isMiscellaneousMathematicalSymbolsB   :: Char -> Bool
isMiscellaneousMathematicalSymbolsB c = c >= '\x2980' && c <= '\x29FF'

isSupplementalMathematicalOperators   :: Char -> Bool
isSupplementalMathematicalOperators c = c >= '\x2A00' && c <= '\x2AFF'

isMiscellaneousSymbolsandArrows   :: Char -> Bool
isMiscellaneousSymbolsandArrows c = c >= '\x2B00' && c <= '\x2BFF'

isGlagolitic   :: Char -> Bool
isGlagolitic c = c >= '\x2C00' && c <= '\x2C5F'

isLatinExtendedC   :: Char -> Bool
isLatinExtendedC c = c >= '\x2C60' && c <= '\x2C7F'

isCoptic   :: Char -> Bool
isCoptic c = c >= '\x2C80' && c <= '\x2CFF'

isGeorgianSupplement   :: Char -> Bool
isGeorgianSupplement c = c >= '\x2D00' && c <= '\x2D2F'

isTifinagh   :: Char -> Bool
isTifinagh c = c >= '\x2D30' && c <= '\x2D7F'

isEthiopicExtended   :: Char -> Bool
isEthiopicExtended c = c >= '\x2D80' && c <= '\x2DDF'

isCyrillicExtendedA   :: Char -> Bool
isCyrillicExtendedA c = c >= '\x2DE0' && c <= '\x2DFF'

isSupplementalPunctuation   :: Char -> Bool
isSupplementalPunctuation c = c >= '\x2E00' && c <= '\x2E7F'

isCJKRadicalsSupplement   :: Char -> Bool
isCJKRadicalsSupplement c = c >= '\x2E80' && c <= '\x2EFF'

isKangxiRadicals   :: Char -> Bool
isKangxiRadicals c = c >= '\x2F00' && c <= '\x2FDF'

isIdeographicDescriptionCharacters   :: Char -> Bool
isIdeographicDescriptionCharacters c = c >= '\x2FF0' && c <= '\x2FFF'

isCJKSymbolsandPunctuation   :: Char -> Bool
isCJKSymbolsandPunctuation c = c >= '\x3000' && c <= '\x303F'

isHiragana   :: Char -> Bool
isHiragana c = c >= '\x3040' && c <= '\x309F'

isKatakana   :: Char -> Bool
isKatakana c = c >= '\x30A0' && c <= '\x30FF'

isBopomofo   :: Char -> Bool
isBopomofo c = c >= '\x3100' && c <= '\x312F'

isHangulCompatibilityJamo   :: Char -> Bool
isHangulCompatibilityJamo c = c >= '\x3130' && c <= '\x318F'

isKanbun   :: Char -> Bool
isKanbun c = c >= '\x3190' && c <= '\x319F'

isBopomofoExtended   :: Char -> Bool
isBopomofoExtended c = c >= '\x31A0' && c <= '\x31BF'

isCJKStrokes   :: Char -> Bool
isCJKStrokes c = c >= '\x31C0' && c <= '\x31EF'

isKatakanaPhoneticExtensions   :: Char -> Bool
isKatakanaPhoneticExtensions c = c >= '\x31F0' && c <= '\x31FF'

isEnclosedCJKLettersandMonths   :: Char -> Bool
isEnclosedCJKLettersandMonths c = c >= '\x3200' && c <= '\x32FF'

isCJKCompatibility   :: Char -> Bool
isCJKCompatibility c = c >= '\x3300' && c <= '\x33FF'

isCJKUnifiedIdeographsExtensionA   :: Char -> Bool
isCJKUnifiedIdeographsExtensionA c = c >= '\x3400' && c <= '\x4DBF'

isYijingHexagramSymbols   :: Char -> Bool
isYijingHexagramSymbols c = c >= '\x4DC0' && c <= '\x4DFF'

isCJKUnifiedIdeographs   :: Char -> Bool
isCJKUnifiedIdeographs c = c >= '\x4E00' && c <= '\x9FFF'

isYiSyllables   :: Char -> Bool
isYiSyllables c = c >= '\xA000' && c <= '\xA48F'

isYiRadicals   :: Char -> Bool
isYiRadicals c = c >= '\xA490' && c <= '\xA4CF'

isLisu   :: Char -> Bool
isLisu c = c >= '\xA4D0' && c <= '\xA4FF'

isVai   :: Char -> Bool
isVai c = c >= '\xA500' && c <= '\xA63F'

isCyrillicExtendedB   :: Char -> Bool
isCyrillicExtendedB c = c >= '\xA640' && c <= '\xA69F'

isBamum   :: Char -> Bool
isBamum c = c >= '\xA6A0' && c <= '\xA6FF'

isModifierToneLetters   :: Char -> Bool
isModifierToneLetters c = c >= '\xA700' && c <= '\xA71F'

isLatinExtendedD   :: Char -> Bool
isLatinExtendedD c = c >= '\xA720' && c <= '\xA7FF'

isSylotiNagri   :: Char -> Bool
isSylotiNagri c = c >= '\xA800' && c <= '\xA82F'

isCommonIndicNumberForms   :: Char -> Bool
isCommonIndicNumberForms c = c >= '\xA830' && c <= '\xA83F'

isPhagspa   :: Char -> Bool
isPhagspa c = c >= '\xA840' && c <= '\xA87F'

isSaurashtra   :: Char -> Bool
isSaurashtra c = c >= '\xA880' && c <= '\xA8DF'

isDevanagariExtended   :: Char -> Bool
isDevanagariExtended c = c >= '\xA8E0' && c <= '\xA8FF'

isKayahLi   :: Char -> Bool
isKayahLi c = c >= '\xA900' && c <= '\xA92F'

isRejang   :: Char -> Bool
isRejang c = c >= '\xA930' && c <= '\xA95F'

isHangulJamoExtendedA   :: Char -> Bool
isHangulJamoExtendedA c = c >= '\xA960' && c <= '\xA97F'

isJavanese   :: Char -> Bool
isJavanese c = c >= '\xA980' && c <= '\xA9DF'

isMyanmarExtendedB   :: Char -> Bool
isMyanmarExtendedB c = c >= '\xA9E0' && c <= '\xA9FF'

isCham   :: Char -> Bool
isCham c = c >= '\xAA00' && c <= '\xAA5F'

isMyanmarExtendedA   :: Char -> Bool
isMyanmarExtendedA c = c >= '\xAA60' && c <= '\xAA7F'

isTaiViet   :: Char -> Bool
isTaiViet c = c >= '\xAA80' && c <= '\xAADF'

isMeeteiMayekExtensions   :: Char -> Bool
isMeeteiMayekExtensions c = c >= '\xAAE0' && c <= '\xAAFF'

isEthiopicExtendedA   :: Char -> Bool
isEthiopicExtendedA c = c >= '\xAB00' && c <= '\xAB2F'

isLatinExtendedE   :: Char -> Bool
isLatinExtendedE c = c >= '\xAB30' && c <= '\xAB6F'

isCherokeeSupplement   :: Char -> Bool
isCherokeeSupplement c = c >= '\xAB70' && c <= '\xABBF'

isMeeteiMayek   :: Char -> Bool
isMeeteiMayek c = c >= '\xABC0' && c <= '\xABFF'

isHangulSyllables   :: Char -> Bool
isHangulSyllables c = c >= '\xAC00' && c <= '\xD7AF'

isHangulJamoExtendedB   :: Char -> Bool
isHangulJamoExtendedB c = c >= '\xD7B0' && c <= '\xD7FF'

isHighSurrogates   :: Char -> Bool
isHighSurrogates c = c >= '\xD800' && c <= '\xDB7F'

isHighPrivateUseSurrogates   :: Char -> Bool
isHighPrivateUseSurrogates c = c >= '\xDB80' && c <= '\xDBFF'

isLowSurrogates   :: Char -> Bool
isLowSurrogates c = c >= '\xDC00' && c <= '\xDFFF'

isPrivateUseArea   :: Char -> Bool
isPrivateUseArea c = c >= '\xE000' && c <= '\xF8FF'

isCJKCompatibilityIdeographs   :: Char -> Bool
isCJKCompatibilityIdeographs c = c >= '\xF900' && c <= '\xFAFF'

isAlphabeticPresentationForms   :: Char -> Bool
isAlphabeticPresentationForms c = c >= '\xFB00' && c <= '\xFB4F'

isArabicPresentationFormsA   :: Char -> Bool
isArabicPresentationFormsA c = c >= '\xFB50' && c <= '\xFDFF'

isVariationSelectors   :: Char -> Bool
isVariationSelectors c = c >= '\xFE00' && c <= '\xFE0F'

isVerticalForms   :: Char -> Bool
isVerticalForms c = c >= '\xFE10' && c <= '\xFE1F'

isCombiningHalfMarks   :: Char -> Bool
isCombiningHalfMarks c = c >= '\xFE20' && c <= '\xFE2F'

isCJKCompatibilityForms   :: Char -> Bool
isCJKCompatibilityForms c = c >= '\xFE30' && c <= '\xFE4F'

isSmallFormVariants   :: Char -> Bool
isSmallFormVariants c = c >= '\xFE50' && c <= '\xFE6F'

isArabicPresentationFormsB   :: Char -> Bool
isArabicPresentationFormsB c = c >= '\xFE70' && c <= '\xFEFF'

isHalfwidthandFullwidthForms   :: Char -> Bool
isHalfwidthandFullwidthForms c = c >= '\xFF00' && c <= '\xFFEF'

isSpecials   :: Char -> Bool
isSpecials c = c >= '\xFFF0' && c <= '\xFFFF'

isLinearBSyllabary   :: Char -> Bool
isLinearBSyllabary c = c >= '\x10000' && c <= '\x1007F'

isLinearBIdeograms   :: Char -> Bool
isLinearBIdeograms c = c >= '\x10080' && c <= '\x100FF'

isAegeanNumbers   :: Char -> Bool
isAegeanNumbers c = c >= '\x10100' && c <= '\x1013F'

isAncientGreekNumbers   :: Char -> Bool
isAncientGreekNumbers c = c >= '\x10140' && c <= '\x1018F'

isAncientSymbols   :: Char -> Bool
isAncientSymbols c = c >= '\x10190' && c <= '\x101CF'

isPhaistosDisc   :: Char -> Bool
isPhaistosDisc c = c >= '\x101D0' && c <= '\x101FF'

isLycian   :: Char -> Bool
isLycian c = c >= '\x10280' && c <= '\x1029F'

isCarian   :: Char -> Bool
isCarian c = c >= '\x102A0' && c <= '\x102DF'

isCopticEpactNumbers   :: Char -> Bool
isCopticEpactNumbers c = c >= '\x102E0' && c <= '\x102FF'

isOldItalic   :: Char -> Bool
isOldItalic c = c >= '\x10300' && c <= '\x1032F'

isGothic   :: Char -> Bool
isGothic c = c >= '\x10330' && c <= '\x1034F'

isOldPermic   :: Char -> Bool
isOldPermic c = c >= '\x10350' && c <= '\x1037F'

isUgaritic   :: Char -> Bool
isUgaritic c = c >= '\x10380' && c <= '\x1039F'

isOldPersian   :: Char -> Bool
isOldPersian c = c >= '\x103A0' && c <= '\x103DF'

isDeseret   :: Char -> Bool
isDeseret c = c >= '\x10400' && c <= '\x1044F'

isShavian   :: Char -> Bool
isShavian c = c >= '\x10450' && c <= '\x1047F'

isOsmanya   :: Char -> Bool
isOsmanya c = c >= '\x10480' && c <= '\x104AF'

isOsage   :: Char -> Bool
isOsage c = c >= '\x104B0' && c <= '\x104FF'

isElbasan   :: Char -> Bool
isElbasan c = c >= '\x10500' && c <= '\x1052F'

isCaucasianAlbanian   :: Char -> Bool
isCaucasianAlbanian c = c >= '\x10530' && c <= '\x1056F'

isLinearA   :: Char -> Bool
isLinearA c = c >= '\x10600' && c <= '\x1077F'

isCypriotSyllabary   :: Char -> Bool
isCypriotSyllabary c = c >= '\x10800' && c <= '\x1083F'

isImperialAramaic   :: Char -> Bool
isImperialAramaic c = c >= '\x10840' && c <= '\x1085F'

isPalmyrene   :: Char -> Bool
isPalmyrene c = c >= '\x10860' && c <= '\x1087F'

isNabataean   :: Char -> Bool
isNabataean c = c >= '\x10880' && c <= '\x108AF'

isHatran   :: Char -> Bool
isHatran c = c >= '\x108E0' && c <= '\x108FF'

isPhoenician   :: Char -> Bool
isPhoenician c = c >= '\x10900' && c <= '\x1091F'

isLydian   :: Char -> Bool
isLydian c = c >= '\x10920' && c <= '\x1093F'

isMeroiticHieroglyphs   :: Char -> Bool
isMeroiticHieroglyphs c = c >= '\x10980' && c <= '\x1099F'

isMeroiticCursive   :: Char -> Bool
isMeroiticCursive c = c >= '\x109A0' && c <= '\x109FF'

isKharoshthi   :: Char -> Bool
isKharoshthi c = c >= '\x10A00' && c <= '\x10A5F'

isOldSouthArabian   :: Char -> Bool
isOldSouthArabian c = c >= '\x10A60' && c <= '\x10A7F'

isOldNorthArabian   :: Char -> Bool
isOldNorthArabian c = c >= '\x10A80' && c <= '\x10A9F'

isManichaean   :: Char -> Bool
isManichaean c = c >= '\x10AC0' && c <= '\x10AFF'

isAvestan   :: Char -> Bool
isAvestan c = c >= '\x10B00' && c <= '\x10B3F'

isInscriptionalParthian   :: Char -> Bool
isInscriptionalParthian c = c >= '\x10B40' && c <= '\x10B5F'

isInscriptionalPahlavi   :: Char -> Bool
isInscriptionalPahlavi c = c >= '\x10B60' && c <= '\x10B7F'

isPsalterPahlavi   :: Char -> Bool
isPsalterPahlavi c = c >= '\x10B80' && c <= '\x10BAF'

isOldTurkic   :: Char -> Bool
isOldTurkic c = c >= '\x10C00' && c <= '\x10C4F'

isOldHungarian   :: Char -> Bool
isOldHungarian c = c >= '\x10C80' && c <= '\x10CFF'

isHanifiRohingya   :: Char -> Bool
isHanifiRohingya c = c >= '\x10D00' && c <= '\x10D3F'

isRumiNumeralSymbols   :: Char -> Bool
isRumiNumeralSymbols c = c >= '\x10E60' && c <= '\x10E7F'

isOldSogdian   :: Char -> Bool
isOldSogdian c = c >= '\x10F00' && c <= '\x10F2F'

isSogdian   :: Char -> Bool
isSogdian c = c >= '\x10F30' && c <= '\x10F6F'

isElymaic   :: Char -> Bool
isElymaic c = c >= '\x10FE0' && c <= '\x10FFF'

isBrahmi   :: Char -> Bool
isBrahmi c = c >= '\x11000' && c <= '\x1107F'

isKaithi   :: Char -> Bool
isKaithi c = c >= '\x11080' && c <= '\x110CF'

isSoraSompeng   :: Char -> Bool
isSoraSompeng c = c >= '\x110D0' && c <= '\x110FF'

isChakma   :: Char -> Bool
isChakma c = c >= '\x11100' && c <= '\x1114F'

isMahajani   :: Char -> Bool
isMahajani c = c >= '\x11150' && c <= '\x1117F'

isSharada   :: Char -> Bool
isSharada c = c >= '\x11180' && c <= '\x111DF'

isSinhalaArchaicNumbers   :: Char -> Bool
isSinhalaArchaicNumbers c = c >= '\x111E0' && c <= '\x111FF'

isKhojki   :: Char -> Bool
isKhojki c = c >= '\x11200' && c <= '\x1124F'

isMultani   :: Char -> Bool
isMultani c = c >= '\x11280' && c <= '\x112AF'

isKhudawadi   :: Char -> Bool
isKhudawadi c = c >= '\x112B0' && c <= '\x112FF'

isGrantha   :: Char -> Bool
isGrantha c = c >= '\x11300' && c <= '\x1137F'

isNewa   :: Char -> Bool
isNewa c = c >= '\x11400' && c <= '\x1147F'

isTirhuta   :: Char -> Bool
isTirhuta c = c >= '\x11480' && c <= '\x114DF'

isSiddham   :: Char -> Bool
isSiddham c = c >= '\x11580' && c <= '\x115FF'

isModi   :: Char -> Bool
isModi c = c >= '\x11600' && c <= '\x1165F'

isMongolianSupplement   :: Char -> Bool
isMongolianSupplement c = c >= '\x11660' && c <= '\x1167F'

isTakri   :: Char -> Bool
isTakri c = c >= '\x11680' && c <= '\x116CF'

isAhom   :: Char -> Bool
isAhom c = c >= '\x11700' && c <= '\x1173F'

isDogra   :: Char -> Bool
isDogra c = c >= '\x11800' && c <= '\x1184F'

isWarangCiti   :: Char -> Bool
isWarangCiti c = c >= '\x118A0' && c <= '\x118FF'

isNandinagari   :: Char -> Bool
isNandinagari c = c >= '\x119A0' && c <= '\x119FF'

isZanabazarSquare   :: Char -> Bool
isZanabazarSquare c = c >= '\x11A00' && c <= '\x11A4F'

isSoyombo   :: Char -> Bool
isSoyombo c = c >= '\x11A50' && c <= '\x11AAF'

isPauCinHau   :: Char -> Bool
isPauCinHau c = c >= '\x11AC0' && c <= '\x11AFF'

isBhaiksuki   :: Char -> Bool
isBhaiksuki c = c >= '\x11C00' && c <= '\x11C6F'

isMarchen   :: Char -> Bool
isMarchen c = c >= '\x11C70' && c <= '\x11CBF'

isMasaramGondi   :: Char -> Bool
isMasaramGondi c = c >= '\x11D00' && c <= '\x11D5F'

isGunjalaGondi   :: Char -> Bool
isGunjalaGondi c = c >= '\x11D60' && c <= '\x11DAF'

isMakasar   :: Char -> Bool
isMakasar c = c >= '\x11EE0' && c <= '\x11EFF'

isTamilSupplement   :: Char -> Bool
isTamilSupplement c = c >= '\x11FC0' && c <= '\x11FFF'

isCuneiform   :: Char -> Bool
isCuneiform c = c >= '\x12000' && c <= '\x123FF'

isCuneiformNumbersandPunctuation   :: Char -> Bool
isCuneiformNumbersandPunctuation c = c >= '\x12400' && c <= '\x1247F'

isEarlyDynasticCuneiform   :: Char -> Bool
isEarlyDynasticCuneiform c = c >= '\x12480' && c <= '\x1254F'

isEgyptianHieroglyphs   :: Char -> Bool
isEgyptianHieroglyphs c = c >= '\x13000' && c <= '\x1342F'

isEgyptianHieroglyphFormatControls   :: Char -> Bool
isEgyptianHieroglyphFormatControls c = c >= '\x13430' && c <= '\x1343F'

isAnatolianHieroglyphs   :: Char -> Bool
isAnatolianHieroglyphs c = c >= '\x14400' && c <= '\x1467F'

isBamumSupplement   :: Char -> Bool
isBamumSupplement c = c >= '\x16800' && c <= '\x16A3F'

isMro   :: Char -> Bool
isMro c = c >= '\x16A40' && c <= '\x16A6F'

isBassaVah   :: Char -> Bool
isBassaVah c = c >= '\x16AD0' && c <= '\x16AFF'

isPahawhHmong   :: Char -> Bool
isPahawhHmong c = c >= '\x16B00' && c <= '\x16B8F'

isMedefaidrin   :: Char -> Bool
isMedefaidrin c = c >= '\x16E40' && c <= '\x16E9F'

isMiao   :: Char -> Bool
isMiao c = c >= '\x16F00' && c <= '\x16F9F'

isIdeographicSymbolsandPunctuation   :: Char -> Bool
isIdeographicSymbolsandPunctuation c = c >= '\x16FE0' && c <= '\x16FFF'

isTangut   :: Char -> Bool
isTangut c = c >= '\x17000' && c <= '\x187FF'

isTangutComponents   :: Char -> Bool
isTangutComponents c = c >= '\x18800' && c <= '\x18AFF'

isKanaSupplement   :: Char -> Bool
isKanaSupplement c = c >= '\x1B000' && c <= '\x1B0FF'

isKanaExtendedA   :: Char -> Bool
isKanaExtendedA c = c >= '\x1B100' && c <= '\x1B12F'

isSmallKanaExtension   :: Char -> Bool
isSmallKanaExtension c = c >= '\x1B130' && c <= '\x1B16F'

isNushu   :: Char -> Bool
isNushu c = c >= '\x1B170' && c <= '\x1B2FF'

isDuployan   :: Char -> Bool
isDuployan c = c >= '\x1BC00' && c <= '\x1BC9F'

isShorthandFormatControls   :: Char -> Bool
isShorthandFormatControls c = c >= '\x1BCA0' && c <= '\x1BCAF'

isByzantineMusicalSymbols   :: Char -> Bool
isByzantineMusicalSymbols c = c >= '\x1D000' && c <= '\x1D0FF'

isMusicalSymbols   :: Char -> Bool
isMusicalSymbols c = c >= '\x1D100' && c <= '\x1D1FF'

isAncientGreekMusicalNotation   :: Char -> Bool
isAncientGreekMusicalNotation c = c >= '\x1D200' && c <= '\x1D24F'

isMayanNumerals   :: Char -> Bool
isMayanNumerals c = c >= '\x1D2E0' && c <= '\x1D2FF'

isTaiXuanJingSymbols   :: Char -> Bool
isTaiXuanJingSymbols c = c >= '\x1D300' && c <= '\x1D35F'

isCountingRodNumerals   :: Char -> Bool
isCountingRodNumerals c = c >= '\x1D360' && c <= '\x1D37F'

isMathematicalAlphanumericSymbols   :: Char -> Bool
isMathematicalAlphanumericSymbols c = c >= '\x1D400' && c <= '\x1D7FF'

isSuttonSignWriting   :: Char -> Bool
isSuttonSignWriting c = c >= '\x1D800' && c <= '\x1DAAF'

isGlagoliticSupplement   :: Char -> Bool
isGlagoliticSupplement c = c >= '\x1E000' && c <= '\x1E02F'

isNyiakengPuachueHmong   :: Char -> Bool
isNyiakengPuachueHmong c = c >= '\x1E100' && c <= '\x1E14F'

isWancho   :: Char -> Bool
isWancho c = c >= '\x1E2C0' && c <= '\x1E2FF'

isMendeKikakui   :: Char -> Bool
isMendeKikakui c = c >= '\x1E800' && c <= '\x1E8DF'

isAdlam   :: Char -> Bool
isAdlam c = c >= '\x1E900' && c <= '\x1E95F'

isIndicSiyaqNumbers   :: Char -> Bool
isIndicSiyaqNumbers c = c >= '\x1EC70' && c <= '\x1ECBF'

isOttomanSiyaqNumbers   :: Char -> Bool
isOttomanSiyaqNumbers c = c >= '\x1ED00' && c <= '\x1ED4F'

isArabicMathematicalAlphabeticSymbols   :: Char -> Bool
isArabicMathematicalAlphabeticSymbols c = c >= '\x1EE00' && c <= '\x1EEFF'

isMahjongTiles   :: Char -> Bool
isMahjongTiles c = c >= '\x1F000' && c <= '\x1F02F'

isDominoTiles   :: Char -> Bool
isDominoTiles c = c >= '\x1F030' && c <= '\x1F09F'

isPlayingCards   :: Char -> Bool
isPlayingCards c = c >= '\x1F0A0' && c <= '\x1F0FF'

isEnclosedAlphanumericSupplement   :: Char -> Bool
isEnclosedAlphanumericSupplement c = c >= '\x1F100' && c <= '\x1F1FF'

isEnclosedIdeographicSupplement   :: Char -> Bool
isEnclosedIdeographicSupplement c = c >= '\x1F200' && c <= '\x1F2FF'

isMiscellaneousSymbolsandPictographs   :: Char -> Bool
isMiscellaneousSymbolsandPictographs c = c >= '\x1F300' && c <= '\x1F5FF'

isEmoticons   :: Char -> Bool
isEmoticons c = c >= '\x1F600' && c <= '\x1F64F'

isOrnamentalDingbats   :: Char -> Bool
isOrnamentalDingbats c = c >= '\x1F650' && c <= '\x1F67F'

isTransportandMapSymbols   :: Char -> Bool
isTransportandMapSymbols c = c >= '\x1F680' && c <= '\x1F6FF'

isAlchemicalSymbols   :: Char -> Bool
isAlchemicalSymbols c = c >= '\x1F700' && c <= '\x1F77F'

isGeometricShapesExtended   :: Char -> Bool
isGeometricShapesExtended c = c >= '\x1F780' && c <= '\x1F7FF'

isSupplementalArrowsC   :: Char -> Bool
isSupplementalArrowsC c = c >= '\x1F800' && c <= '\x1F8FF'

isSupplementalSymbolsandPictographs   :: Char -> Bool
isSupplementalSymbolsandPictographs c = c >= '\x1F900' && c <= '\x1F9FF'

isChessSymbols   :: Char -> Bool
isChessSymbols c = c >= '\x1FA00' && c <= '\x1FA6F'

isSymbolsandPictographsExtendedA   :: Char -> Bool
isSymbolsandPictographsExtendedA c = c >= '\x1FA70' && c <= '\x1FAFF'

isCJKUnifiedIdeographsExtensionB   :: Char -> Bool
isCJKUnifiedIdeographsExtensionB c = c >= '\x20000' && c <= '\x2A6DF'

isCJKUnifiedIdeographsExtensionC   :: Char -> Bool
isCJKUnifiedIdeographsExtensionC c = c >= '\x2A700' && c <= '\x2B73F'

isCJKUnifiedIdeographsExtensionD   :: Char -> Bool
isCJKUnifiedIdeographsExtensionD c = c >= '\x2B740' && c <= '\x2B81F'

isCJKUnifiedIdeographsExtensionE   :: Char -> Bool
isCJKUnifiedIdeographsExtensionE c = c >= '\x2B820' && c <= '\x2CEAF'

isCJKUnifiedIdeographsExtensionF   :: Char -> Bool
isCJKUnifiedIdeographsExtensionF c = c >= '\x2CEB0' && c <= '\x2EBEF'

isCJKCompatibilityIdeographsSupplement   :: Char -> Bool
isCJKCompatibilityIdeographsSupplement c = c >= '\x2F800' && c <= '\x2FA1F'

isTags   :: Char -> Bool
isTags c = c >= '\xE0000' && c <= '\xE007F'

isVariationSelectorsSupplement   :: Char -> Bool
isVariationSelectorsSupplement c = c >= '\xE0100' && c <= '\xE01EF'

isSupplementaryPrivateUseAreaA   :: Char -> Bool
isSupplementaryPrivateUseAreaA c = c >= '\xF0000' && c <= '\xFFFFF'

isSupplementaryPrivateUseAreaB   :: Char -> Bool
isSupplementaryPrivateUseAreaB c = c >= '\x100000' && c <= '\x10FFFF'

-- ------------------------------------------------------------

