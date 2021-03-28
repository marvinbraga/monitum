unit uShowMessage;
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
  TMessageSize = (Small, Medium, Large);

  IFormMessage = interface
    ['{770585A0-33AC-457E-94AC-A11B45E2D349}']
    procedure ShowMessage;
    procedure CloseMessage;
  end;

implementation

end.
