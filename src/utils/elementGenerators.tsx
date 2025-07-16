import { JSX } from 'react'
import {
  RequisitoExpressao,
  RequisitoBase,
  isRequisitoOR,
  isRequisitoAND,
  isRequisitoPoder
} from '../types/poderes'
import { getTipoPoderPorNome } from './getPoder'
import {
  formatarRequisitoBaseTexto,
  nomeParaID,
  gerarIdRequisito
} from './formatters'

const MAX_RECURSION_DEPTH = 50

/**
 * Cria um elemento de requisito individual
 * VERSÃO ATUALIZADA com type guards
 */
const criarElementoRequisito = (
  item: RequisitoBase,
  index: number,
  styles: Record<string, string>
): JSX.Element => {
  const tipoClass = `req-${item.tipo}`
  let estilo: string | undefined
  let especial: string | undefined

  if (isRequisitoPoder(item)) {
    estilo = getTipoPoderPorNome(item.nome)
    especial = item.especial
  }

  const texto = formatarRequisitoBaseTexto(item)
  const shouldUseLink = estilo && !especial

  return (
    <div
      key={gerarIdRequisito(item, index)}
      className={`${styles[tipoClass]} ${
        especial ? styles[`tipo-${especial}`] : styles[`tipo-${estilo}`]
      }`}
    >
      {shouldUseLink ? (
        <a href={`#${nomeParaID(texto)}`} aria-label={`Ir para ${texto}`}>
          {texto}
        </a>
      ) : (
        texto
      )}
    </div>
  )
}

/**
 * Processa requisitos do tipo OR
 */
const processarRequisitosOR = (
  expressao: { OR: RequisitoExpressao[] },
  styles: Record<string, string>,
  depth: number
): JSX.Element => {
  const filhos = expressao.OR.map((item, index) => {
    if (Array.isArray(item) || isRequisitoOR(item) || isRequisitoAND(item)) {
      return gerarElementosRequisitos(item, styles, depth + 1)
    }
    return criarElementoRequisito(item, index, styles)
  })

  return (
    <div key={`or-group-${depth}`} className={styles.grupoOR}>
      <div className={styles.requisito}>{filhos}</div>
    </div>
  )
}

/**
 * Processa requisitos do tipo AND
 */
const processarRequisitosAND = (
  expressao: { AND: RequisitoExpressao[] },
  styles: Record<string, string>,
  depth: number
): JSX.Element => {
  const filhos = expressao.AND.map((item, index) => {
    if (Array.isArray(item) || isRequisitoOR(item) || isRequisitoAND(item)) {
      return gerarElementosRequisitos(item, styles, depth + 1)
    }
    return criarElementoRequisito(item, index, styles)
  })

  return (
    <div key={`and-group-${depth}`} className={styles.grupoAND}>
      <div className={styles.requisito}>{filhos}</div>
    </div>
  )
}

/**
 * Função recursiva para gerar os elementos JSX dos requisitos
 * VERSÃO ATUALIZADA com suporte a AND e melhor tipagem
 */
export const gerarElementosRequisitos = (
  expressao: RequisitoExpressao,
  styles: Record<string, string>,
  depth: number = 0
): JSX.Element[] => {
  // Proteção contra recursão infinita
  if (depth > MAX_RECURSION_DEPTH) {
    console.warn('Profundidade máxima de recursão atingida para requisitos')
    return [
      <div key='max-depth-warning' className={styles.warning}>
        Estrutura de requisitos muito complexa
      </div>
    ]
  }

  const elementos: JSX.Element[] = []

  if (Array.isArray(expressao)) {
    // Array = AND implícito
    expressao.forEach((item, index) => {
      elementos.push(...gerarElementosRequisitos(item, styles, depth + 1))
    })
  } else if (isRequisitoOR(expressao)) {
    // OR explícito
    elementos.push(processarRequisitosOR(expressao, styles, depth))
  } else if (isRequisitoAND(expressao)) {
    // AND explícito
    elementos.push(processarRequisitosAND(expressao, styles, depth))
  } else {
    // Requisito único
    elementos.push(
      <div key={`single-req-${depth}`} className={styles.requisito}>
        {criarElementoRequisito(expressao, depth, styles)}
      </div>
    )
  }

  return elementos
}
