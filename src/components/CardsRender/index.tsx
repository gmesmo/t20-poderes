import { useContext } from 'react'
import { FilterContext, FilterType } from '../../context/Filter'
import { Poder, RequisitoExpressao } from '../../types/poderes'
import Card from '../Card'

import styles from './styles.module.scss'

// Função auxiliar para verificar se um requisito específico existe na estrutura
const verificarRequisitoNaEstrutura = (
  estrutura: any,
  requisitoProcurado: string
): boolean => {
  if (!estrutura) return false

  if (Array.isArray(estrutura)) {
    // Array = AND implícito
    return estrutura.some((item) =>
      verificarRequisitoNaEstrutura(item, requisitoProcurado)
    )
  } else if (estrutura.OR) {
    // OR explícito
    return estrutura.OR.some((item: RequisitoExpressao) =>
      verificarRequisitoNaEstrutura(item, requisitoProcurado)
    )
  } else if (estrutura.AND) {
    // AND explícito
    return estrutura.AND.some((item: RequisitoExpressao) =>
      verificarRequisitoNaEstrutura(item, requisitoProcurado)
    )
  } else if (typeof estrutura === 'object' && estrutura.tipo) {
    // Requisito individual - comparar como JSON
    return JSON.stringify(estrutura) === requisitoProcurado
  }

  return false
}

const isFiltered = (poder: Poder, filtro: FilterType) => {
  if (
    filtro.nome &&
    !poder.nome.toLowerCase().includes(filtro.nome.toLowerCase())
  )
    return false

  if (filtro.tipo.length > 0 && !filtro.tipo.includes(poder.tipo)) return false

  if (filtro.requisitos.length > 0) {
    // Verificar se pelo menos um dos requisitos selecionados está presente no poder
    const temRequisito = filtro.requisitos.some((requisito) =>
      verificarRequisitoNaEstrutura(poder.requisitos, requisito)
    )

    if (!temRequisito) {
      return false
    }
  }

  return true
}

function CardsRender({ poderes }: { poderes: Poder[] }) {
  const context = useContext(FilterContext)

  if (!context) return null

  const { filter } = context

  return (
    <div className={styles.cardsWrapper}>
      {poderes
        .filter((card) => isFiltered(card, filter))
        .map((card) => (
          <Card key={card.nome} poder={card} />
        ))}
    </div>
  )
}

export default CardsRender
