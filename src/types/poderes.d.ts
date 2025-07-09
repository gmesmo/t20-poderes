// Tipos para os requisitos base
export interface RequisitoAtributo {
  tipo: 'atributo'
  nome: string // Ex: "Des", "For"
  valor: number // Valor mínimo
}

export interface RequisitoPericia {
  tipo: 'pericia'
  nome: string // Ex: "Misticismo", "Religiao"
  treinado: boolean // true se precisa ser treinado, false se não treinado
  descricao?: string // Descrição opcional da perícia
}

export interface RequisitoPoder {
  tipo: 'poder'
  nome: string // Nome do poder requerido
  treinado: boolean // true se precisa ser treinado, false se não treinado
}

export interface RequisitoProficiencia {
  tipo: 'proficiencia'
  nome: string // Nome da proficiência requerida
}

export interface RequisitoNivel {
  tipo: 'nivel'
  valor: number // Nível mínimo
}

// União de todos os tipos de requisitos base
export type RequisitoBase =
  | RequisitoAtributo
  | RequisitoPericia
  | RequisitoPoder
  | RequisitoProficiencia
  | RequisitoNivel

// Expressões lógicas
export interface RequisitoOR {
  OR: RequisitoExpressao[]
}

// Aqui não usamos interface RequisitoAND, pois o AND será implícito com array de RequisitoBase

// Tipo que define a estrutura completa de uma expressão de requisito
export type RequisitoExpressao =
  | RequisitoBase[] // AND implícito
  | RequisitoBase // Também permitimos um único requisito direto
  | RequisitoOR // OR explícito

export interface Ref {
  Manual: string // Nome do manual, ex: "Guia do Jogador"
  pagina: number // Página do manual
}

// Interface principal para o Poder
export interface Poder {
  nome: string
  subtitulo?: string
  tipo: string
  requisitos?: RequisitoExpressao
  texto: string
  tabela?: {
    headers: string[]
    rows: (string | number)[][]
  }
  ref: Ref
}
