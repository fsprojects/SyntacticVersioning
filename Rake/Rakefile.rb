require "tmpdir"
require "fileutils"
require "securerandom"
require "nuget_helper"

def magnitude_next_nuget_version(package_name, next_dll)
  begin
    tmp = File.join(Dir.tmpdir, SecureRandom.hex)
    NugetHelper.exec("install '#{package_name}' -o #{tmp} ")
    orig = Dir.glob( File.join(tmp, "**", File.basename(next_dll)) ).first
    path_to_package = Dir.glob( File.join(tmp, "*") ).first 
    v = SemVer.parse(path_to_package)
    NugetHelper.run_tool_with_result(NugetHelper.command_path('SynVer','synver.exe'), " --bump #{v} #{orig} #{next_dll}").strip
  ensure
    FileUtils.rm_rf(tmp)
  end
end


desc "Install missing NuGet packages."
task :restore do
  NugetHelper.run_tool_with_result("../.paket/paket.exe", "restore")
end

task :bump, [:restore] do
    v = magnitude_next_nuget_version "SynVer.Lib", "../src/SynVer.Lib/bin/Debug/SynVer.Lib.dll"
    # use v to set the version
    puts "Next version is #{v} !\n"
end

task :default do

end