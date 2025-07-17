import { createContext, useState, ReactNode } from 'react'

export type FilterType = {
  nome: string
  tipo: string
  requisitos: string[]
  ref: string
}

type FilterContextType = {
  filter: FilterType
  setFilter: React.Dispatch<React.SetStateAction<FilterType>>
}

export const FilterContext = createContext<FilterContextType | undefined>(
  undefined
)

export function FilterProvider({ children }: { children: ReactNode }) {
  const [filter, setFilter] = useState<FilterType>({
    nome: '',
    tipo: '',
    requisitos: [],
    ref: ''
  })

  return (
    <FilterContext.Provider value={{ filter, setFilter }}>
      {children}
    </FilterContext.Provider>
  )
}
