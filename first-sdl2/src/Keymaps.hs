module Keymaps (
  azerty_on_qwerty
  , qwerty_on_qwerty
)
where

import qualified SDL


type Keymap = [(SDL.Keycode, Char)]


azerty_on_qwerty :: SDL.Keycode -> Maybe Char
azerty_on_qwerty = flip lookup azerty_on_qwerty_data


azerty_on_qwerty_data :: Keymap
azerty_on_qwerty_data = [
  (SDL.KeycodeQ, 'a')
  , (SDL.KeycodeW, 'z')
  , (SDL.KeycodeE, 'e')
  , (SDL.KeycodeR, 'r')
  , (SDL.KeycodeT, 't')
  , (SDL.KeycodeY, 'y')
  , (SDL.KeycodeU, 'u')
  , (SDL.KeycodeI, 'i')
  , (SDL.KeycodeO, 'o')
  , (SDL.KeycodeP, 'p')
  , (SDL.KeycodeA, 'q')
  , (SDL.KeycodeS, 's')
  , (SDL.KeycodeD, 'd')
  , (SDL.KeycodeF, 'f')
  , (SDL.KeycodeG, 'g')
  , (SDL.KeycodeH, 'h')
  , (SDL.KeycodeJ, 'j')
  , (SDL.KeycodeK, 'k')
  , (SDL.KeycodeL, 'l')
  , (SDL.KeycodeZ, 'w')
  , (SDL.KeycodeX, 'x')
  , (SDL.KeycodeC, 'c')
  , (SDL.KeycodeV, 'v')
  , (SDL.KeycodeB, 'b')
  , (SDL.KeycodeN, 'n')
  , (SDL.KeycodeSemicolon, 'm')]


qwerty_on_qwerty :: SDL.Keycode -> Maybe Char
qwerty_on_qwerty = flip lookup qwerty_on_qwerty_data


qwerty_on_qwerty_data :: Keymap
qwerty_on_qwerty_data = [
  (SDL.KeycodeQ, 'q')
  , (SDL.KeycodeW, 'w')
  , (SDL.KeycodeE, 'e')
  , (SDL.KeycodeR, 'r')
  , (SDL.KeycodeT, 't')
  , (SDL.KeycodeY, 'y')
  , (SDL.KeycodeU, 'u')
  , (SDL.KeycodeI, 'i')
  , (SDL.KeycodeO, 'o')
  , (SDL.KeycodeP, 'p')
  , (SDL.KeycodeA, 'a')
  , (SDL.KeycodeS, 's')
  , (SDL.KeycodeD, 'd')
  , (SDL.KeycodeF, 'f')
  , (SDL.KeycodeG, 'g')
  , (SDL.KeycodeH, 'h')
  , (SDL.KeycodeJ, 'j')
  , (SDL.KeycodeK, 'k')
  , (SDL.KeycodeL, 'l')
  , (SDL.KeycodeZ, 'z')
  , (SDL.KeycodeX, 'x')
  , (SDL.KeycodeC, 'c')
  , (SDL.KeycodeV, 'v')
  , (SDL.KeycodeB, 'b')
  , (SDL.KeycodeN, 'n')
  , (SDL.KeycodeM, 'm')]
