import styles from './styles.module.scss'

const Footer = () => {
  return (
    <footer className={styles.footer}>
      <div className={styles.legalWrapper}>
        <p>
          <b>Aviso Legal: </b>Este site é uma iniciativa não oficial,
          desenvolvida por fãs, sem qualquer finalidade lucrativa ou comercial.
          Todo o conteúdo relacionado a Tormenta20 é propriedade intelectual da
          Jambô Editora, sendo utilizado neste espaço de forma não autorizada,
          apenas para fins informativos e de apreciação da obra original, nos
          termos do uso justo (fair use).
        </p>
      </div>
    </footer>
  )
}

export default Footer
