import { JSX, useMemo } from 'react'
import { RequisitoExpressao } from '../../types/poderes'
import { gerarElementosRequisitos } from '../../utils/elementGenerators'

interface CardRequisitosProps {
  requisitos: RequisitoExpressao
  styles: Record<string, string>
}

const CardRequisitos = ({
  requisitos,
  styles
}: CardRequisitosProps): JSX.Element => {
  // Memoriza a geração de elementos para evitar recálculos desnecessários
  const elementosRequisitos = useMemo(() => {
    return gerarElementosRequisitos(requisitos, styles)
  }, [requisitos, styles])

  return (
    <div className={styles.requisitos}>
      <strong>Requer:</strong>
      {elementosRequisitos}
    </div>
  )
}

export default CardRequisitos
