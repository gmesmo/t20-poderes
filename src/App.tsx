import CardsRender from './components/CardsRender'
import poderesData from './assets/poderes.json' // Importa o JSON diretamente
import { Poder } from './types/poderes' // Importa o tipo Poder
import Footer from './components/Footer'
import Filter from './components/Filter'

import styles from './styles.module.scss'

function App() {
  // O TypeScript deve inferir poderesData como Poder[] se o JSON estiver bem tipado
  // Mas para garantir, podemos fazer um cast ou afirmar o tipo
  const poderes: Poder[] = poderesData as Poder[]

  return (
    <div className='App'>
      <div className={styles.container}>
        <Filter poderes={poderes} />
        <CardsRender poderes={poderes} />
      </div>
      <Footer />
    </div>
  )
}

export default App
