%define _app     eni
%define _version 0.0.1
%define _release 1
%define _packager Stanislaw Klekot <dozzie@jarowit.net>

%define _erlang_lib_dir %(erl -noshell -eval "io:fwrite(code:lib_dir())" -s erlang halt)

Summary: INI-like configuration loader for Erlang
Name: erlang-%{_app}
Version: %{_version}
Release: %{_release}%{?dist}
License: Apache 2.0
Group: Development/Tools
URL: http://dozzie.jarowit.net/trac/wiki/eni
Source0: %{name}-%{version}.tar.gz
BuildRoot: %{_tmppath}/%{name}-%{version}-%{release}-root-%(%{__id_u} -n)
Requires: erlang
BuildArch: noarch
BuildRequires: erlang-devel
BuildRequires: rebar

%define _app_libdir  %{_erlang_lib_dir}/%{_app}-%{version}
%define _app_ebin    %{_app_libdir}/ebin
%define _app_priv    %{_app_libdir}/priv
%define _app_include %{_app_libdir}/include
%define _app_src     %{_app_libdir}/src
%define _app_doc     %{_app_libdir}/doc

%description
INI-like configuration loader for Erlang.

%package doc
Summary: INI-like configuration loader for Erlang - API documentation
Group: Documentation

%description doc
INI-like configuration loader for Erlang.

This package contains API documentation generated out of ENI's sources.

%package source
Summary: INI-like configuration loader for Erlang - sources
Group: Development/Languages

%description source
INI-like configuration loader for Erlang.

This package contains ENI sources.

%prep
%setup -q

%build
rebar compile
rebar doc

%install
rm -rf $RPM_BUILD_ROOT

mkdir -p $RPM_BUILD_ROOT%{_app_libdir}
mkdir    $RPM_BUILD_ROOT%{_app_ebin}
#mkdir    $RPM_BUILD_ROOT%{_app_priv}
#mkdir    $RPM_BUILD_ROOT%{_app_include}
mkdir    $RPM_BUILD_ROOT%{_app_src}
mkdir    $RPM_BUILD_ROOT%{_app_doc}
cp    ebin/*.app ebin/*.beam $RPM_BUILD_ROOT%{_app_ebin}
#cp -r priv/*                 $RPM_BUILD_ROOT%{_app_priv}
#cp -r include/*              $RPM_BUILD_ROOT%{_app_include}
cp -r src/*                  $RPM_BUILD_ROOT%{_app_src}
cp -r doc/*                  $RPM_BUILD_ROOT%{_app_doc}

%clean
rm -rf $RPM_BUILD_ROOT

%files
%defattr(-,root,root,-)
%doc COPYING
%exclude %{_app_doc}
%exclude %{_app_src}
%{_app_libdir}

%files doc
%defattr(-,root,root)
%{_app_doc}

%files source
%defattr(-,root,root)
%{_app_src}

#%changelog
