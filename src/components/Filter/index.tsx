/* eslint-disable react-hooks/rules-of-hooks */
import { useContext, useEffect, useState } from 'react'
import React from 'react'

import { Poder, RequisitoExpressao } from '../../types/poderes'
import { FilterContext } from '../../context/Filter'
import { gerarElementosRequisitos } from '../../utils/elementGenerators'

import styles from './styles.module.scss'
import { getTipoPoderPorNome } from '../../utils/getPoder'

const Filter = ({ poderes }: { poderes: Poder[] }) => {
  const context = useContext(FilterContext)

  if (!context) return null

  const { filter, setFilter } = context
  const [inputValue, setInputValue] = useState(() => filter.nome)
  const [requisitosOpen, setRequisitosOpen] = useState(false)

  useEffect(() => {
    setInputValue(filter.nome)
  }, [filter.nome])

  useEffect(() => {
    const handler = setTimeout(() => {
      setFilter((f) => ({ ...f, nome: inputValue }))
    }, 500)

    return () => clearTimeout(handler)
  }, [inputValue, setFilter])

  // Função auxiliar para extrair requisitos individuais de uma expressão
  const extrairRequisitosIndividuais = (
    expressao: any,
    requisitosSet: Set<string>
  ) => {
    if (!expressao) return

    if (Array.isArray(expressao)) {
      // Array = AND implícito
      expressao.forEach((item) =>
        extrairRequisitosIndividuais(item, requisitosSet)
      )
    } else if (expressao.OR) {
      // OR explícito
      expressao.OR.forEach((item: RequisitoExpressao) =>
        extrairRequisitosIndividuais(item, requisitosSet)
      )
    } else if (expressao.AND) {
      // AND explícito
      expressao.AND.forEach((item: RequisitoExpressao) =>
        extrairRequisitosIndividuais(item, requisitosSet)
      )
    } else if (typeof expressao === 'object' && expressao.tipo) {
      // Requisito individual - criar um identificador único
      const id = JSON.stringify(expressao)
      requisitosSet.add(id)
    }
  }

  // Função para extrair todos os requisitos únicos com ordenação melhorada
  const getTodosRequisitos = () => {
    const requisitosSet = new Set<string>()

    poderes.forEach((poder) => {
      if (poder.requisitos) {
        extrairRequisitosIndividuais(poder.requisitos, requisitosSet)
      }
    })

    const requisitos = Array.from(requisitosSet)

    // Função de comparação personalizada
    const compareRequisitos = (a: string, b: string) => {
      try {
        const reqA = JSON.parse(a)
        const reqB = JSON.parse(b)

        // Verificar se é Tormenta primeiro
        const isTormentaA =
          reqA.especial === 'Tormenta' ||
          (reqA.tipo === 'poder' &&
            reqA.nome &&
            getTipoPoderPorNome(reqA.nome) === 'Tormenta')
        const isTormentaB =
          reqB.especial === 'Tormenta' ||
          (reqB.tipo === 'poder' &&
            reqB.nome &&
            getTipoPoderPorNome(reqB.nome) === 'Tormenta')

        // Se um é Tormenta e o outro não, Tormenta vai para o final
        if (isTormentaA && !isTormentaB) return 1
        if (!isTormentaA && isTormentaB) return -1

        // Se ambos são Tormenta, ordena alfabeticamente
        if (isTormentaA && isTormentaB) {
          return (reqA.nome || '').localeCompare(reqB.nome || '')
        }

        // Para requisitos não-Tormenta, ordena por tipo
        const tipoA: string = reqA.tipo || 'zzz'
        const tipoB: string = reqB.tipo || 'zzz'

        if (tipoA !== tipoB) {
          // Define uma ordem específica para os tipos
          const ordemTipos: Record<
            | 'atributo'
            | 'pericia'
            | 'proficiencia'
            | 'nivel'
            | 'poder'
            | 'especial',
            number
          > = {
            atributo: 1,
            pericia: 2,
            proficiencia: 3,
            nivel: 4,
            poder: 5,
            especial: 6
          }

          const prioridadeA =
            ordemTipos[tipoA as keyof typeof ordemTipos] ?? 999
          const prioridadeB =
            ordemTipos[tipoB as keyof typeof ordemTipos] ?? 999

          return prioridadeA - prioridadeB
        }

        // Se são do mesmo tipo, aplica ordenação específica
        if (tipoA === 'atributo' || tipoA === 'pericia') {
          // Para atributos e perícias, ordena por nome e depois por valor
          const nomeComparison = (reqA.nome || '').localeCompare(
            reqB.nome || ''
          )
          if (nomeComparison !== 0) return nomeComparison

          // Se têm o mesmo nome, ordena por valor numérico
          const valorA = parseInt(reqA.valor) || 0
          const valorB = parseInt(reqB.valor) || 0
          return valorA - valorB
        }

        if (tipoA === 'nivel') {
          // Para níveis, ordena numericamente
          const valorA = parseInt(reqA.valor) || 0
          const valorB = parseInt(reqB.valor) || 0
          return valorA - valorB
        }

        if (tipoA === 'poder') {
          // Para poderes, ordena alfabeticamente por nome
          return (reqA.nome || '').localeCompare(reqB.nome || '')
        }

        if (tipoA === 'proficiencia') {
          // Para proficiências, ordena por categoria e depois por nome
          const categoriaComparison = (reqA.categoria || '').localeCompare(
            reqB.categoria || ''
          )
          if (categoriaComparison !== 0) return categoriaComparison

          return (reqA.nome || '').localeCompare(reqB.nome || '')
        }

        // Para outros tipos, ordena alfabeticamente pela string JSON
        return a.localeCompare(b)
      } catch (error) {
        // Se não conseguir fazer parse, ordena como string
        return a.localeCompare(b)
      }
    }

    requisitos.sort(compareRequisitos)
    return requisitos
  }

  const todosRequisitos = getTodosRequisitos()

  const handleRequisitoChange = (requisito: string, checked: boolean) => {
    setFilter((f) => ({
      ...f,
      requisitos: checked
        ? [...(f.requisitos || []), requisito]
        : (f.requisitos || []).filter((r) => r !== requisito)
    }))
  }

  // Função para renderizar um requisito individual no filtro
  const renderRequisitoFilter = (requisitoJson: string, index: number) => {
    try {
      const requisito = JSON.parse(requisitoJson)
      // Usar a função gerarElementosRequisitos para obter o JSX
      const elementos = gerarElementosRequisitos(requisito, styles, 0, true)

      // Garantir chaves únicas para cada elemento no contexto do filtro
      return elementos.map((elemento, idx) =>
        React.cloneElement(elemento, {
          key: `filter-req-${index}-${idx}`
        })
      )
    } catch (error) {
      return requisitoJson
    }
  }

  const styleHandler = (requisito: RequisitoExpressao) => {
    if (
      requisito &&
      typeof requisito === 'object' &&
      !Array.isArray(requisito) &&
      'tipo' in requisito &&
      (requisito.tipo === 'atributo' ||
        requisito.tipo === 'pericia' ||
        requisito.tipo === 'proficiencia' ||
        requisito.tipo === 'nivel')
    ) {
      return `req-${requisito.tipo}` // atributo
    } else if (
      requisito &&
      typeof requisito === 'object' &&
      !Array.isArray(requisito) &&
      'especial' in requisito &&
      requisito.especial === 'Tormenta'
    ) {
      return 'tipo-Tormenta'
    } else if (
      requisito &&
      typeof requisito === 'object' &&
      !Array.isArray(requisito) &&
      'tipo' in requisito &&
      'nome' in requisito &&
      requisito.tipo === 'poder'
    ) {
      return `tipo-${getTipoPoderPorNome(requisito.nome)}`
    }
  }

  return (
    <div id={styles.filter}>
      <form>
        <input
          className={styles.textFilter}
          type='text'
          placeholder='Filtrar'
          value={inputValue}
          onChange={(e) => setInputValue(e.target.value)}
        />

        <select
          value={filter.tipo}
          onChange={(e) =>
            setFilter((f) => ({
              ...f,
              tipo: e.target.value
            }))
          }
        >
          <option value=''>Todos</option>
          {Array.from(new Set(poderes.map((poder) => poder.tipo))).map(
            (tipo) => (
              <option key={tipo} value={tipo}>
                {tipo}
              </option>
            )
          )}
        </select>

        {/* Seção de filtros de requisitos */}
        <div className={styles.requisitosFilter}>
          <h4 onClick={() => setRequisitosOpen(!requisitosOpen)}>Requisitos</h4>
          <div
            className={`${styles.requisitosWrapper} ${
              requisitosOpen ? styles.open : ''
            }`}
          >
            {todosRequisitos.map((requisito, index) => (
              <label
                key={`filter-label-${index}`}
                className={`${styles.checkboxLabel} ${styleHandler(
                  JSON.parse(requisito)
                )}`}
              >
                <input
                  type='checkbox'
                  checked={filter.requisitos?.includes(requisito) || false}
                  onChange={(e) =>
                    handleRequisitoChange(requisito, e.target.checked)
                  }
                />
                <span className={styles.requisitoText}>
                  {renderRequisitoFilter(requisito, index)}
                </span>
              </label>
            ))}
          </div>
        </div>
      </form>
    </div>
  )
}

export default Filter
