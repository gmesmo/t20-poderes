// src/components/Card.tsx
import { JSX, useEffect, useState } from 'react'
import * as BsIcons from 'react-icons/bs'

import { Poder, RequisitoExpressao, RequisitoBase } from '../../types/poderes'

import styles from './styles.module.scss'
import { Pericia, Atributo } from '../../assets/enums'
import { getTipoPoderPorNome } from '../../utils/getPoder'
import { Tooltip } from '@mui/material'

// Função para formatar um requisito base em uma string
const formatarRequisitoBaseTexto = (req: RequisitoBase): string => {
  if (req.tipo === 'atributo') {
    const atributoNome = req.nome as keyof typeof Atributo

    return `${Atributo[atributoNome]} ${req.valor}`
  }
  if (req.tipo === 'pericia') {
    const periciaNome = req.nome as keyof typeof Pericia

    return `${Pericia[periciaNome]} ${req.descricao ? req.descricao : ''}`
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
          <a href={`#${nomeParaID(formatarRequisitoBaseTexto(item))}`}>
            {formatarRequisitoBaseTexto(item)}
          </a>
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
    let especial: string | undefined

    if (expressao.tipo === 'poder') {
      const poderNome = expressao.nome
      especial = expressao.especial

      estilo = getTipoPoderPorNome(poderNome)
    }

    elementos.push(
      <div key={JSON.stringify(expressao)} className={styles.requisito}>
        <div
          className={`${styles[tipoClass]} ${
            especial ? styles[`tipo-${especial}`] : styles[`tipo-${estilo}`]
          }`}
        >
          {estilo && !especial ? (
            <a href={`#${nomeParaID(formatarRequisitoBaseTexto(expressao))}`}>
              {formatarRequisitoBaseTexto(expressao)}
            </a>
          ) : (
            formatarRequisitoBaseTexto(expressao)
          )}
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

const nomeParaID = (nome: string) => {
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

function Card({ poder }: CardProps) {
  const { nome, subtitulo, tipo, requisitos, texto, ref } = poder

  const Icon = BsIcons.BsStars as React.ComponentType<{ className?: string }>

  const [open, setOpen] = useState(false)

  useEffect(() => {
    let timeoutId: NodeJS.Timeout
    if (open) {
      timeoutId = setTimeout(() => setOpen(false), 2000)
    }
    return () => clearTimeout(timeoutId)
  }, [open])

  return (
    <div id={nomeParaID(nome)} className={styles.card}>
      <div
        className={`${styles.titulo} ${
          styles[`tipo-${gerarNomeTipo(tipo || '')}`]
        } ${!subtitulo && styles.semSubtitulo}`}
      >
        <span className={styles.nomeWrapper}>
          {nome}
          {poder.efeitoMagico && (
            <Tooltip title='Efeito mágico' placement='top' arrow>
              <Icon className={styles.efeitoMagico} />
            </Tooltip>
          )}
        </span>
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
        {poder.custo && (
          <p className={styles.custo}>
            <strong>Custo:</strong> +{poder.custo}
          </p>
        )}
        <Tooltip title='Texto copiado!' placement='top-end' open={open} arrow>
          <div className={styles.textoWrapper} onClick={() => setOpen(true)}>
            {formataTexto(texto)}
          </div>
        </Tooltip>
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
