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
  treinado?: boolean // true se precisa ser treinado, false se não treinado
  especial?: string // Tipo especial do poder (usado para estilização)
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

export interface RequisitoAND {
  AND: RequisitoExpressao[]
}

// Tipo que define a estrutura completa de uma expressão de requisito
export type RequisitoExpressao =
  | RequisitoBase[] // AND implícito
  | RequisitoBase // Também permitimos um único requisito direto
  | RequisitoOR // OR explícito
  | RequisitoAND // AND explícito (adicionado para compatibilidade)

export interface Ref {
  Manual: string // Nome do manual, ex: "Guia do Jogador"
  pagina: number // Página do manual
}

// Interface principal para o Poder
export interface Poder {
  nome: string
  subtitulo?: string
  efeitoMagico?: boolean // Indica se é um poder mágico
  tipo: string
  custo?: string // Mantido como string conforme seu tipo original
  requisitos?: RequisitoExpressao
  texto: string
  tabela?: {
    headers: string[]
    rows: (string | number)[][]
  }
  ref: Ref
}

// ============================================================================
// UTILITÁRIOS DE TIPO - Type Guards
// ============================================================================

/**
 * Verifica se um requisito é do tipo atributo
 */
export const isRequisitoAtributo = (
  req: RequisitoBase
): req is RequisitoAtributo => {
  return req.tipo === 'atributo'
}

/**
 * Verifica se um requisito é do tipo perícia
 */
export const isRequisitoPericia = (
  req: RequisitoBase
): req is RequisitoPericia => {
  return req.tipo === 'pericia'
}

/**
 * Verifica se um requisito é do tipo poder
 */
export const isRequisitoPoder = (req: RequisitoBase): req is RequisitoPoder => {
  return req.tipo === 'poder'
}

/**
 * Verifica se um requisito é do tipo proficiência
 */
export const isRequisitoProficiencia = (
  req: RequisitoBase
): req is RequisitoProficiencia => {
  return req.tipo === 'proficiencia'
}

/**
 * Verifica se um requisito é do tipo nível
 */
export const isRequisitoNivel = (req: RequisitoBase): req is RequisitoNivel => {
  return req.tipo === 'nivel'
}

/**
 * Verifica se uma expressão é do tipo OR
 */
export const isRequisitoOR = (
  expr: RequisitoExpressao
): expr is RequisitoOR => {
  return typeof expr === 'object' && !Array.isArray(expr) && 'OR' in expr
}

/**
 * Verifica se uma expressão é do tipo AND
 */
export const isRequisitoAND = (
  expr: RequisitoExpressao
): expr is RequisitoAND => {
  return typeof expr === 'object' && !Array.isArray(expr) && 'AND' in expr
}
