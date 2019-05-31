{-# LANGUAGE OverloadedStrings          #-}

module Tool.Constant where
import ClassyPrelude
import Data.ByteString.Internal
import GHC.Word
{-
*
-}

{-
* 密码加密的盐值
-}
originalsalt::ByteString
originalsalt = "hnbrina2019XN9dUU8uhnbrina2019bQSkvEZIRhnbrina2019UWr9UVWCjzOLsU=hnbrina2019LbmItlhltyIHhnbrina20194Nro2YyMFeCCKwtV0=hnbrina2019"

{-
* 与数据库连接有关
* dbport 数据库端口
* dpip 数据库ip
* dpuser 数据库用户名
* dppwd 数据库密码
* dbbase 数据库名称
-}
dbport=3306::Word16

dpip="localhost"
-- dpip="192.168.0.134"

dpuser="root"

dppwd="Hnbrina2019."

dbbase = "codingame"

-- normalState=0::Int

-- abnormalState=1::Int


languages=[
    ("Bash","7c16c986-4dea-4322-8e04-2c49801ac43e"),
    ("C","5894ab5e-6fc8-494d-acb0-dabd6522f270"),
    ("C#","54e1f5d2-9b48-4e92-b331-f07974238b5a"),
    ("C++","04b18036-887e-48c6-a641-3b8f8aff6991"),
    ("Clojure","a2dcc130-8746-479c-a8a3-68eab47bf6a9"),
    ("Dart","5a76d027-5530-43c0-853a-ce818ffbe4b7"),
    ("F#","f3ee5c2e-e572-48af-8b20-b66a37eb77dc"),
    ("Go","3334f9aa-552b-4456-969a-c78defad3adf"),
    ("Groovy","9229a081-adc9-4bb7-9350-0c27da38e53b"),
    ("Haskell","817e420a-1b16-4500-9ac6-3a9aeb8d2d09"),
    ("Java","a3b83ecd-dcdf-4b3f-9f2c-c9ce2f982c65"),
    ("Javascript Kotlin","b9e30bb1-5550-46ac-8573-202069505ac7"),
    ("Lua","b315dbd8-f576-4da6-8a32-f7810ca2f7a2"),
    ("Objective OCaml","6e389d2c-82f3-42b9-a051-183e6f9ac325"),
    ("Pascal","517b6ebd-9e62-4292-9f00-7be8452d1701"),
    ("Perl","8bb5aa8f-490e-46d2-8ef6-84b14e6b454c"),
    ("PHP","e73c5822-abf0-4839-adfa-6adfba4b1dd3"),
    ("Python","5d916a25-72c8-414e-bd16-091076e92a40"),
    ("Python3 ","faf338cb-80fd-445d-b345-77c09c6d8581"),
    ("Ruby","46d88142-a3e9-4254-8976-24d36403baf9"),
    ("Rust","2b2fd064-9487-4313-8056-5a7b3e176ed6"),
    ("Scala","f8067b43-099e-4420-97bb-c5356ea7619c"),
    ("Swift","9358fb6e-0bb3-4829-8845-d8cc15e8052d"),
    ("VB.NET","c93105b4-2e39-4aef-81eb-8ea6141ba60b")
    ]

admin="fe95c189-5720-49d6-9059-8c94dd373fd2"