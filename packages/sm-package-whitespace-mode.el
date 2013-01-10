;;;; Package whitespace-mode
(sm-package whitespace-mode
            :package-manager nil
            :unmanaged-p t)

(require 'whitespace)
(setq whitespace-display-mappings
      '(
        (space-mark 32 [183] [46]) ; normal space
        (space-mark 160 [164] [95])
        (space-mark 2208 [2212] [95])
        (space-mark 2336 [2340] [95])
        (space-mark 3616 [3620] [95])
        (space-mark 3872 [3876] [95])
        (newline-mark 10 [182 10]) ; newline
        (tab-mark 9 [9655 9] [92 9]) ; tab
        ))
(sm-provide :package whitespace-mode)
