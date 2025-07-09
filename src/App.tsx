// src/App.tsx
import CardsRender from './components/CardsRender'
import poderesData from './assets/poderes.json' // Importa o JSON diretamente
import { Poder } from './types/poderes' // Importa o tipo Poder

function App() {
  // O TypeScript deve inferir poderesData como Poder[] se o JSON estiver bem tipado
  // Mas para garantir, podemos fazer um cast ou afirmar o tipo
  const poderes: Poder[] = poderesData as Poder[]

  return (
    <div className='App'>
      <CardsRender cards={poderes} />
    </div>
  )
}

export default App
