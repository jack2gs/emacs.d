;; custom-vars.el

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes 'nil)
 '(package-selected-packages nil)
 '(safe-local-variable-values
   '((dape-configs
      (my_app modes
	      (c-mode c-ts-mode c++-mode c++-ts-mode)
	      command-args
	      ("--port" :autoport)
	      ensure dape-ensure-command command-cwd dape-command-cwd command "lldb-dap" port :autoport :type "lldb" :request "launch" :cwd "build" :program "build/my_app" :args
	      []
	      :stopOnEntry nil)
      (my_app1 modes
	       (c-mode c-ts-mode c++-mode c++-ts-mode)
	       command-args
	       ("--port" :autoport)
	       ensure dape-ensure-command command-cwd dape-command-cwd command "lldb-dap" port :autoport :type "lldb" :request "launch" :cwd "build" :program "build/my_app" :args
	       []
	       :stopOnEntry nil)))))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

