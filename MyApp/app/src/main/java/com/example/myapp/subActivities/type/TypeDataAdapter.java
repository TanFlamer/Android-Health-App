package com.example.myapp.subActivities.type;

import android.content.Context;
import android.view.View;
import android.view.ViewGroup;
import android.widget.ArrayAdapter;
import android.widget.TextView;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;

import com.example.myapp.databasefiles.type.Type;

import java.util.List;

public class TypeDataAdapter extends ArrayAdapter<Type> {

    private final List<Type> typeList;

    public TypeDataAdapter(@NonNull Context context, int resource, List<Type> typeList) {
        super(context, resource, typeList);
        this.typeList = typeList;
    }

    @NonNull
    @Override
    public View getView(int position, @Nullable View convertView, @NonNull ViewGroup parent) {
        TextView label = (TextView) super.getView(position, convertView, parent);
        label.setText(typeList.get(position).getTypeName());
        return label;
    }

    @Override
    public View getDropDownView(int position, @Nullable View convertView, @NonNull ViewGroup parent) {
        TextView label = (TextView) super.getView(position, convertView, parent);
        label.setText(typeList.get(position).getTypeName());
        return label;
    }
}
