// src/components/Card.tsx
import { JSX } from 'react'
import { Poder, RequisitoExpressao, RequisitoBase } from '../../types/poderes'

import styles from './styles.module.scss'
import { Pericia, Atributo } from '../../assets/enums'
import { getTipoPoderPorNome } from '../../utils/getPoder'

// Função para formatar um requisito base em uma string
const formatarRequisitoBaseTexto = (req: RequisitoBase): string => {
  if (req.tipo === 'atributo') {
    const atributoNome = req.nome as keyof typeof Atributo

    return `${Atributo[atributoNome]} ${req.valor}`
  }
  if (req.tipo === 'pericia') {
    const periciaNome = req.nome as keyof typeof Pericia

    return `${Pericia[periciaNome]}`
  }
  if (req.tipo === 'poder') {
    return `${req.nome}`
  }
  if (req.tipo === 'proficiencia') {
    return `${req.nome}`
  }
  if (req.tipo === 'nivel') {
    return `Nvl ${req.valor}`
  }
  return ''
}

// Função recursiva para gerar os elementos JSX dos requisitos
const gerarElementosRequisitos = (
  expressao: RequisitoExpressao
): JSX.Element[] => {
  const elementos: JSX.Element[] = []

  if (Array.isArray(expressao)) {
    expressao.forEach((item) => {
      elementos.push(...gerarElementosRequisitos(item))
    })
  } else if ('OR' in expressao) {
    const filhos = expressao.OR.map((item) => {
      if (Array.isArray(item) || 'OR' in item || 'AND' in item) {
        return gerarElementosRequisitos(item)
      }

      const tipoClass = `req-${item.tipo}`
      let estilo: string | undefined

      if (item.tipo === 'poder') {
        estilo = getTipoPoderPorNome(item.nome)
      }

      return (
        <div
          key={JSON.stringify(item)}
          className={`${styles[tipoClass]} ${
            estilo && styles[`tipo-${estilo}`]
          }`}
        >
          {formatarRequisitoBaseTexto(item)}
        </div>
      )
    })

    elementos.push(
      <div key={JSON.stringify(expressao)} className={styles.grupoOR}>
        <div className={styles.requisito}>{filhos}</div>
      </div>
    )
  } else {
    const tipoClass = `req-${expressao.tipo}`
    let estilo: string | undefined

    if (expressao.tipo === 'poder') {
      const poderNome = expressao.nome
      estilo = getTipoPoderPorNome(poderNome)
      console.log(`Estilo do poder "${poderNome}": ${estilo}`)
    }

    elementos.push(
      <div key={JSON.stringify(expressao)} className={styles.requisito}>
        <div
          className={`${styles[tipoClass]} ${
            estilo && styles[`tipo-${estilo}`]
          }`}
        >
          {formatarRequisitoBaseTexto(expressao)}
        </div>
      </div>
    )
  }

  return elementos
}

interface CardProps {
  poder: Poder
}

function gerarNomeTipo(tipo: string): string {
  switch (tipo) {
    case 'Poder Concedido':
      return 'PoderConcedido'
    default:
      return tipo
  }
}

function formataTexto(texto: string): JSX.Element[] {
  return texto.split('\n').map((linha, index) => (
    <p key={index} className={styles.texto}>
      {linha}
    </p>
  ))
}

function Card({ poder }: CardProps) {
  const { nome, subtitulo, tipo, requisitos, texto, ref } = poder

  return (
    <div className={styles.card}>
      <div
        className={`${styles.titulo} ${
          styles[`tipo-${gerarNomeTipo(tipo || '')}`]
        } ${!subtitulo && styles.semSubtitulo}`}
      >
        {nome}
        {subtitulo && <h4 className={styles.subtitulo}>{subtitulo}</h4>}
      </div>
      <div className={styles.content}>
        {tipo && (
          <p className='tipo-poder'>
            Poder {tipo === 'Poder Concedido' ? 'Concedido' : `de ${tipo}`}
          </p>
        )}
        {requisitos && (
          <div className={styles.requisitos}>
            <strong>Requer:</strong>
            {gerarElementosRequisitos(requisitos)}
          </div>
        )}
        {formataTexto(texto)}
        {poder.tabela && (
          <table className={styles.tabela}>
            <thead>
              <tr>
                {poder.tabela.headers.map((header, index) => (
                  <th key={index}>{header}</th>
                ))}
              </tr>
            </thead>
            <tbody>
              {poder.tabela.rows.map((row, rowIndex) => (
                <tr key={rowIndex}>
                  {row.map((cell, cellIndex) => (
                    <td key={cellIndex}>{cell}</td>
                  ))}
                </tr>
              ))}
            </tbody>
          </table>
        )}
      </div>
      <p className={styles.ref}>
        <strong>Referência:</strong> {ref.Manual} p. {ref.pagina}
      </p>
    </div>
  )
}

export default Card
