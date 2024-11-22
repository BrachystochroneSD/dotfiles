(use-package codestral
  :load-path "~/.emacs.d/my-packages/codestral.el"
  :custom
  (codestral-api-key "wlAk18MdR7iCNxoh1cKWwi0z0CoHD4k6")
  (codestral-api-url "https://codestral.mistral.ai")
  :bind (("C-M-:" . global-codestral-mode)
         :map codestral-mode-map
         ("C-:" . codestral-accept-completion)
         ("C-=" . codestral-next-completion)
         ("C-;" . codestral-previous-completion)))
