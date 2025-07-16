import { JSX } from 'react'

interface CardTableProps {
  tabela: {
    headers: string[]
    rows: string[][]
  }
  styles: Record<string, string>
}

const CardTable = ({ tabela, styles }: CardTableProps): JSX.Element => {
  return (
    <table className={styles.tabela}>
      <thead>
        <tr>
          {tabela.headers.map((header, index) => (
            <th key={`header-${index}`}>{header}</th>
          ))}
        </tr>
      </thead>
      <tbody>
        {tabela.rows.map((row, rowIndex) => (
          <tr key={`row-${rowIndex}`}>
            {row.map((cell, cellIndex) => (
              <td key={`cell-${rowIndex}-${cellIndex}`}>{cell}</td>
            ))}
          </tr>
        ))}
      </tbody>
    </table>
  )
}

export default CardTable
