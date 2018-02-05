module Jekyll
  class GFMCodeBlocks < Generator
    def generate(site)
      site.posts.docs.each do |post| rewrite_gfm(post) end
      site.pages.each do |post| rewrite_gfm(post) end
    end

    def rewrite_gfm(entity)
      entity.content.gsub!(/(\n|^)```([^\n]+)\n(.*?)```\n/m,
                           "\\1{% highlight \\2 %}\n\\3{% endhighlight %}\n")
    end
  end
end
