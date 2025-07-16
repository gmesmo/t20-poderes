import { JSX } from 'react'
import { Tooltip } from '@mui/material'
import * as BsIcons from 'react-icons/bs'
import { nomeParaID, gerarNomeTipo } from '../../utils/formatters'

interface CardHeaderProps {
  nome: string
  subtitulo?: string
  tipo?: string
  efeitoMagico?: boolean
  styles: Record<string, string>
}

const CardHeader = ({
  nome,
  subtitulo,
  tipo,
  efeitoMagico,
  styles
}: CardHeaderProps): JSX.Element => {
  const Icon = BsIcons.BsStars as React.ComponentType<{ className?: string }>

  return (
    <div
      className={`${styles.titulo} ${
        styles[`tipo-${gerarNomeTipo(tipo || '')}`]
      } ${!subtitulo ? styles.semSubtitulo : ''}`}
    >
      <span className={styles.nomeWrapper}>
        {nome}
        {efeitoMagico && (
          <Tooltip title='Efeito mágico' placement='top' arrow>
            <Icon className={styles.efeitoMagico} />
          </Tooltip>
        )}
      </span>
      {subtitulo && <h4 className={styles.subtitulo}>{subtitulo}</h4>}
    </div>
  )
}

export default CardHeader
