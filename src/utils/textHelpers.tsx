import { JSX } from 'react'

/**
 * Converte texto com quebras de linha em elementos JSX
 */
export const formataTexto = (
  texto: string,
  className?: string
): JSX.Element[] => {
  return texto.split('\n').map((linha, index) => (
    <p key={`text-line-${index}`} className={className}>
      {linha}
    </p>
  ))
}

/**
 * Trunca texto para determinado comprimento
 */
export const truncarTexto = (texto: string, maxLength: number): string => {
  if (texto.length <= maxLength) return texto
  return texto.substring(0, maxLength) + '...'
}
