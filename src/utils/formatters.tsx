import { RequisitoBase } from '../types/poderes'
import { Pericia, Atributo } from '../assets/enums'
import {
  isRequisitoAtributo,
  isRequisitoPericia,
  isRequisitoPoder,
  isRequisitoProficiencia,
  isRequisitoNivel
} from '../types/poderes'

/**
 * Formata um requisito base em uma string legível
 * VERSÃO ATUALIZADA com type guards
 */
export const formatarRequisitoBaseTexto = (req: RequisitoBase): string => {
  if (isRequisitoAtributo(req)) {
    const atributoNome = req.nome as keyof typeof Atributo
    return `${Atributo[atributoNome]} ${req.valor}`
  }

  if (isRequisitoPericia(req)) {
    const periciaNome = req.nome as keyof typeof Pericia
    return `${Pericia[periciaNome]} ${req.descricao || ''}`
  }

  if (isRequisitoPoder(req)) {
    return req.nome
  }

  if (isRequisitoProficiencia(req)) {
    return req.nome
  }

  if (isRequisitoNivel(req)) {
    return `Nvl ${req.valor}`
  }

  return ''
}

/**
 * Converte nome para ID válido (camelCase)
 */
export const nomeParaID = (nome: string): string => {
  return nome
    .normalize('NFD') // separa acentos de letras
    .replace(/[\u0300-\u036f]/g, '') // remove os acentos
    .replace(/[^a-zA-Z0-9 ]/g, '') // remove caracteres especiais, mas mantém espaços
    .trim()
    .split(' ') // separa palavras
    .map((palavra, index) => {
      if (index === 0) {
        return palavra.toLowerCase()
      }
      return palavra.charAt(0).toUpperCase() + palavra.slice(1).toLowerCase()
    })
    .join('')
}

/**
 * Gera um ID único para um requisito
 */
export const gerarIdRequisito = (req: RequisitoBase, index: number): string => {
  let identificador = ''

  if (isRequisitoAtributo(req)) {
    identificador = `${req.nome || 'unknown'}-${req.valor}`
  } else if (isRequisitoNivel(req)) {
    identificador = `nivel-${req.valor}`
  } else {
    identificador = req.nome || 'unknown'
  }

  return `${req.tipo}-${identificador}-${index}`
}

/**
 * Formata nome do tipo de poder
 */
export const gerarNomeTipo = (tipo: string): string => {
  switch (tipo) {
    case 'Poder Concedido':
      return 'PoderConcedido'
    default:
      return tipo
  }
}
