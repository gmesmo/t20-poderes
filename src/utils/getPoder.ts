// getTipoPoderPorNome.ts
import { Poder } from '../types/poderes'
import poderes from '../assets/poderes.json'

export function getTipoPoderPorNome(nomePoder: string): string | undefined {
  const poder = (poderes as Poder[]).find(
    (p) => p.nome.toLowerCase() === nomePoder.toLowerCase()
  )
  return poder?.tipo
}
