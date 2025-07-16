import { JSX, useMemo } from 'react'
import { Tooltip } from '@mui/material'

import { Poder } from '../../types/poderes'
import { useClipboard } from '../../hooks/useClipboard'
import { nomeParaID } from '../../utils/formatters'
import { formataTexto } from '../../utils/textHelpers'
import CardHeader from './CardHeader'
import CardRequisitos from './CardRequisitos'
import CardTable from './CardTable'

import styles from './styles.module.scss'

interface CardProps {
  poder: Poder
}

const Card = ({ poder }: CardProps): JSX.Element => {
  const { nome, subtitulo, tipo, requisitos, texto, ref } = poder
  const { copied, copy } = useClipboard({ timeout: 2000 })

  // Memoiza o texto formatado
  const textoFormatado = useMemo(() => {
    return formataTexto(texto, styles.texto)
  }, [texto])

  const handleCopyText = async (): Promise<void> => {
    await copy(texto)
  }

  const handleKeyDown = (event: React.KeyboardEvent): void => {
    if (event.key === 'Enter' || event.key === ' ') {
      event.preventDefault()
      handleCopyText()
    }
  }

  return (
    <div id={nomeParaID(nome)} className={styles.card}>
      <CardHeader
        nome={nome}
        subtitulo={subtitulo}
        tipo={tipo}
        efeitoMagico={poder.efeitoMagico}
        styles={styles}
      />

      <div className={styles.content}>
        {tipo && (
          <p className='tipo-poder'>
            Poder {tipo === 'Poder Concedido' ? 'Concedido' : `de ${tipo}`}
          </p>
        )}

        {requisitos && (
          <CardRequisitos requisitos={requisitos} styles={styles} />
        )}

        {poder.custo && (
          <p className={styles.custo}>
            <strong>Custo:</strong> +{poder.custo}
          </p>
        )}

        <Tooltip title='Texto copiado!' placement='top-end' open={copied} arrow>
          <div
            className={styles.textoWrapper}
            onClick={handleCopyText}
            onKeyDown={handleKeyDown}
            role='button'
            tabIndex={0}
            aria-label='Copiar texto do poder'
          >
            {textoFormatado}
          </div>
        </Tooltip>

        {poder.tabela && (
          <CardTable
            tabela={{
              headers: poder.tabela.headers,
              rows: poder.tabela.rows.map((row) =>
                row.map((cell) => String(cell))
              )
            }}
            styles={styles}
          />
        )}
      </div>

      <p className={styles.ref}>
        <strong>Referência:</strong> {ref.Manual} p. {ref.pagina}
      </p>
    </div>
  )
}

export default Card
