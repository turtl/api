ENV['ENV'] ||= 'dev'

set :app,		"www1.beeets.com"
set :gitbranch,	"master"
set :conf,		"dev"

set :application,	"turtl/api"

set :deploy_to,		"/srv/www/turtl/api"
set :deploy_via,	:remote_cache
set :user, 			"deploy"
set :use_sudo,		false
set :keep_releases,	5

set :scm, 			:git
set :repository,	"git@github.com:turtl/api"
set :branch,		gitbranch
set :scm_verbose,	1

#set :gateway, "beeets.com"
#role :db, "beeets.com"
#role :web, "www1.beeets.com", "www2.beeets.com"
#role :app, "www1.beeets.com", "www2.beeets.com", :primary => true
role :web, "www1.beeets.com"
role :app, "www1.beeets.com", :primary => true


ssh_options[:keys] = %w(~/.ssh/id_rsa)
ssh_options[:port] = 50390

namespace :deploy do
	desc "Copy server-specific configuration files, run cleanup, ..."
	task :finalize_update do
		# copy our config files into the includes/ dir
		run("cp #{latest_release}/includes/#{conf}/* #{latest_release}/includes/")
		
		# run our post-update scripts
		run("chmod u+x #{latest_release}/scripts/*")
		run("cd #{latest_release}/scripts && ./prepare")
	end
end

namespace :deploy do
	desc "Restart shit. For now just clears PHP's opcode cache"
	task :restart do
		# clear our opcode cache by 'touch'ing all the code files in the new rerease. 
		# this forces the file mod times to update. note the escaped backslash
		#run("sudo ")
	end
end
