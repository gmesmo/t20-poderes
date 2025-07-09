import { Poder } from '../../types/poderes'
import Card from '../Card'

import styles from './styles.module.scss'

function CardsRender({ cards }: { cards: Poder[] }) {
  return (
    <div className={styles.cardsWrapper}>
      {cards.map((card, index) => (
        <Card key={index} poder={card} />
      ))}
    </div>
  )
}

export default CardsRender
