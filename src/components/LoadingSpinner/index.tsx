import styles from './styles.module.scss'

const LoadingSpinner = () => {
  return (
    <div className={styles.loadingContainer}>
      <div className={styles.spinner}></div>
      <p className={styles.loadingText}>Carregando poderes...</p>
    </div>
  )
}

export default LoadingSpinner

