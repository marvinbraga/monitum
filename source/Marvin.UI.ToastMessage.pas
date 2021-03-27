unit Marvin.UI.ToastMessage;
{
   Monitum Project, 2021.
   Marcus Vinicius Braga, mvbraga@gmail.com.
   https://github.com/marvinbraga/monitum
   GNU General Public License v3.0
   See all terms in:
   https://github.com/marvinbraga/monitum/blob/main/LICENSE
}

interface

type
  IToast = interface
    ['{1CCAF322-7287-4AB2-BA8C-7C56ACD6C979}']
    function Show: IToast;
    procedure Hide;
  end;

  IToastMessage = interface
    ['{3DA6CE96-1952-4DBE-849D-2845ECA621EC}']
    function ShowFrame: IToastMessage;
    function HideFrame: IToastMessage;
  end;

implementation

end.
