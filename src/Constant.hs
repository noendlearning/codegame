{-# LANGUAGE OverloadedStrings          #-}

module Constant (
    originalsalt
    ,dbport
    ,dpip
    ,dpuser
    ,dppwd
    ,dbbase 
    ) where

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

dpuser="root"

dppwd="1"

dbbase = "test"

