class Character {
  constructor(code) {
    this.code = code;
  }
}

export function makeCharacter(code) {
  return new Character(code);
}

export function isCharacter(x) {
  return x instanceof Character;
}

export function charCode(character) {
  character.code;
}
