import { useState, useEffect } from 'react'
import CardsRender from './components/CardsRender'
import { Poder } from './types/poderes' // Importa o tipo Poder
import Footer from './components/Footer'
import Filter from './components/Filter'
import LoadingSpinner from './components/LoadingSpinner'
import ErrorBoundary from './components/ErrorBoundary'
import { FilterProvider } from './context/Filter'
// import ThemeToggle from './components/ThemeToggle'
// import { ThemeProvider } from './context/Theme'

import styles from './styles.module.scss'

function App() {
  const [poderes, setPoderes] = useState<Poder[]>([])
  const [loading, setLoading] = useState<boolean>(true)
  const [error, setError] = useState<string | null>(null)

  useEffect(() => {
    const loadData = async () => {
      try {
        setLoading(true)

        // Carrega múltiplos arquivos JSON de forma assíncrona
        // const [poderesResponse, habilidadesResponse] = await Promise.all([
        const [poderesResponse] = await Promise.all([
          import('./assets/poderes.json')
          // import('./assets/habilidades.json')
        ])

        // Combina os dados dos dois arquivos
        const normalizeRequisitos = (requisitos: any) => {
          if (!requisitos) return undefined
          // Ajusta o tipo para o literal correto
          if (requisitos.tipo === 'poder' || requisitos.tipo === 'pericia') {
            return {
              ...requisitos,
              tipo: requisitos.tipo as 'poder' | 'pericia'
            }
          }
          return requisitos
        }

        const todosPoderes: Poder[] = [
          ...poderesResponse.default.map((p: any) => ({
            ...p,
            requisitos: normalizeRequisitos(p.requisitos)
          }))
          // ...habilidadesResponse.default.map((h: any) => ({
          //   ...h,
          //   requisitos: normalizeRequisitos(h.requisitos)
          // }))
        ]

        setPoderes(todosPoderes)
      } catch (err) {
        setError('Erro ao carregar os dados dos poderes')
        console.error('Erro ao carregar arquivos JSON:', err)
      } finally {
        setLoading(false)
      }
    }

    loadData()
  }, [])

  if (loading) {
    return (
      // <ThemeProvider>
      <LoadingSpinner />
      // </ThemeProvider>
    )
  }

  if (error) {
    return (
      // <ThemeProvider>
      <div className={styles.errorContainer}>
        <h2>Erro</h2>
        <p>{error}</p>
        <button onClick={() => window.location.reload()}>
          Tentar novamente
        </button>
      </div>
      // </ThemeProvider>
    )
  }

  return (
    // <ThemeProvider>
    <ErrorBoundary>
      <FilterProvider>
        <div className='App'>
          {/* <ThemeToggle /> */}
          <div className={styles.container}>
            <Filter poderes={poderes} />
            <CardsRender poderes={poderes} />
          </div>
          <Footer />
        </div>
      </FilterProvider>
    </ErrorBoundary>
    // </ThemeProvider>
  )
}

export default App
