import { useState, useCallback, useRef, useEffect } from 'react'

interface UseClipboardOptions {
  timeout?: number
}

interface UseClipboardReturn {
  copied: boolean
  copy: (text: string) => Promise<boolean>
  reset: () => void
}

/**
 * Hook para gerenciar operações de clipboard
 */
export const useClipboard = (
  options: UseClipboardOptions = {}
): UseClipboardReturn => {
  const { timeout = 2000 } = options
  const [copied, setCopied] = useState(false)
  const timeoutRef = useRef<NodeJS.Timeout | null>(null)

  const copy = useCallback(
    async (text: string): Promise<boolean> => {
      if (!navigator.clipboard) {
        console.warn('Clipboard API não suportada')
        return false
      }

      try {
        await navigator.clipboard.writeText(text)
        setCopied(true)

        // Limpa timeout anterior se existir
        if (timeoutRef.current) {
          clearTimeout(timeoutRef.current)
        }

        // Define novo timeout
        timeoutRef.current = setTimeout(() => {
          setCopied(false)
        }, timeout)

        return true
      } catch (error) {
        console.error('Erro ao copiar texto:', error)
        return false
      }
    },
    [timeout]
  )

  const reset = useCallback(() => {
    setCopied(false)
    if (timeoutRef.current) {
      clearTimeout(timeoutRef.current)
    }
  }, [])

  // Cleanup do timeout quando o componente é desmontado
  useEffect(() => {
    return () => {
      if (timeoutRef.current) {
        clearTimeout(timeoutRef.current)
      }
    }
  }, [])

  return { copied, copy, reset }
}
