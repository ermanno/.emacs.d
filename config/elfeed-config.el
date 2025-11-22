(use-package elfeed
  :ensure t
  :custom
  (elfeed-feeds
   '(("https://www.youtube.com/feeds/videos.xml?channel_id=UCqZe2cxQNCvgkQxUuqsIaig" youtube)
     ))
  (elfeed-search-filter "@1-week-ago +unread")
  :config
  (setq elfeed-show-entry-switch 'display-buffer)
  )

(provide 'elfeed-config)
