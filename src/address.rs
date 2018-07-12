use std::num::Int;
use std::ops::Add;

// The idea here is that it doesn't make sense to add two addresses, but it
// does make sense to add an address and an "address-difference". (If this
// is too annoying to work with we should let it go.)
#[derive(Copy, PartialEq, Eq, PartialOrd, Ord, Debug)]
pub struct AddressDiff(pub i32);

#[derive(Copy, PartialEq, Eq, PartialOrd, Ord, Debug)]
pub struct Address(pub u16);

impl Add<AddressDiff> for Address {
    type Output = Address;

    fn add(self, AddressDiff(rhs): AddressDiff) -> Address {
        let Address(lhs) = self;

        // TODO akeeton: Do a checked cast.
        Address(((lhs as i32) + rhs) as u16)
    }
}

impl Add for AddressDiff {
    type Output = AddressDiff;

    fn add(self, AddressDiff(rhs): AddressDiff) -> AddressDiff {
        let AddressDiff(lhs) = self;
        AddressDiff(lhs + rhs)
    }
}

#[derive(Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct CheckedAddressDiff(u16);

impl Add<CheckedAddressDiff> for Address {
    type Output = Address;

    fn add(self, CheckedAddressDiff(rhs): CheckedAddressDiff) -> Address {
        let Address(lhs) = self;

        // We probably don't want to overflow when doing arithmetic in our own
        // code.
        debug_assert!(lhs.checked_add(rhs).is_some());

        Address(lhs + rhs)
    }
}

impl Address {
    pub fn to_u16(&self) -> u16 {
        match *self {
            Address(address_) => address_
        }
    }

    pub fn to_usize(&self) -> usize {
        self.to_u16() as usize
    }

    pub fn get_page_number(&self) -> u8 {
        (self.to_u16() & 0xff00 >> 8) as u8
    }

    pub fn get_offset(&self) -> u8 {
        (self.to_u16() & 0x00ff) as u8
    }
}